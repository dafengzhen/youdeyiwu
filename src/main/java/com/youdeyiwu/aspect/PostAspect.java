package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PostReviewStateApplicationEvent;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.PointNotFoundException;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.forum.DisableCommentReplyPostDto;
import com.youdeyiwu.model.dto.forum.DisableUserCommentReplyPostDto;
import com.youdeyiwu.model.dto.forum.UpdateStatesPostDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostUserEntity;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.tool.I18nTool;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

/**
 * post.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class PostAspect {

  private final ApplicationEventPublisher publisher;

  private final PostRepository postRepository;

  private final I18nTool i18nTool;

  private final SecurityService securityService;

  private final UserRepository userRepository;

  private final ConfigRepository configRepository;

  /**
   * updateStates.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.updateStates(..)) && args(id,dto)", argNames = "id,dto")
  public void updateStatesPointcut(Long id, UpdateStatesPostDto dto) {
    // Pointcut
  }

  /**
   * disableCommentReply.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.disableCommentReply(..)) && args(id,dto)", argNames = "id,dto")
  public void disableCommentReplyPointcut(Long id, DisableCommentReplyPostDto dto) {
    // Pointcut
  }

  /**
   * disableUserCommentReply.
   */
  @Pointcut(
      value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.disableUserCommentReply(..)) && args(id,userId,dto)", argNames = "id,userId,dto"
  )
  public void disableUserCommentReplyPointcut(Long id, Long userId, DisableUserCommentReplyPostDto dto) {
    // Pointcut
  }

  /**
   * queryDetails.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.queryDetails(..)) && args(pageable,id,postKey)", argNames = "pageable,id,postKey")
  public void queryDetailsPointcut(Pageable pageable, Long id, String postKey) {
    // Pointcut
  }

  /**
   * before advice.
   */
  @Before(value = "queryDetailsPointcut(pageable,id,postKey)", argNames = "pageable,id,postKey")
  public void queryDetailsBeforeAdvice(Pageable pageable, Long id, String postKey) {
    Boolean enable = configRepository.findOptionalByTypeAndName(
            ConfigTypeEnum.POINT,
            PointConfigConstant.ENABLE
        )
        .map(configEntity -> Boolean.valueOf(configEntity.getValue()))
        .orElse(false);

    if (Boolean.FALSE.equals(enable)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(id)
        .orElseThrow(PostNotFoundException::new);
    SectionEntity sectionEntity = postEntity.getSection();

    if (
        Objects.isNull(sectionEntity)
            || Objects.isNull(sectionEntity.getAccessPoints())
            || sectionEntity.getAccessPoints() == 0
    ) {
      return;
    }

    Integer accessPoints = sectionEntity.getAccessPoints();
    if (securityService.isAnonymous()) {
      throw new CustomException(
          i18nTool.getMessage(
              "section.accessPoints.error",
              Map.of(
                  "point", accessPoints,
                  "currentPoint", "0"
              )
          )
      );
    }

    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    PointEntity pointEntity = userEntity.getPoint();
    if (Objects.isNull(pointEntity) || pointEntity.getPoints() < accessPoints) {
      throw new CustomException(
          i18nTool.getMessage(
              "section.accessPoints.error",
              Map.of(
                  "point", accessPoints,
                  "currentPoint", "0"
              )
          )
      );
    }
  }

  /**
   * after advice.
   */
  @After(value = "updateStatesPointcut(id,dto)", argNames = "id,dto")
  public void updateStatesAfterAdvice(Long id, UpdateStatesPostDto dto) {
    PostEntity postEntity = postRepository.getReferenceById(id);

    if (
        Objects.nonNull(dto.reviewState())
            && dto.reviewState() != postEntity.getOldReviewState()
    ) {
      publisher.publishEvent(new PostReviewStateApplicationEvent(postEntity));
    }
  }

  /**
   * after advice.
   */
  @After(value = "disableCommentReplyPointcut(id,dto)", argNames = "id,dto")
  public void disableCommentReplyAfterAdvice(Long id, DisableCommentReplyPostDto dto) {
    PostEntity postEntity = postRepository.findById(id)
        .orElseThrow(PointNotFoundException::new);

    if (
        Objects.nonNull(dto.disableComments())
            && Objects.nonNull(postEntity.getOldDisableComments())
            && !Objects.equals(
            postEntity.getDisableComments(),
            postEntity.getOldDisableComments()
        )
    ) {
      String key;
      String userKey;
      if (Boolean.TRUE.equals(postEntity.getDisableComments())) {
        key = "comment.create.disable.message";
        userKey = "comment.create.disable.user.message";
      } else {
        key = "comment.create.disable.cancel.message";
        userKey = "comment.create.disable.cancel.user.message";
      }

      sendDisableCommentReplyMessage(
          key,
          dto.commentDisableReason(),
          postEntity
      );
      sendDisableCommentReplyUserMessage(
          userKey,
          dto.commentDisableReason(),
          postEntity
      );
    }

    if (
        Objects.nonNull(dto.disableReplies())
            && Objects.nonNull(postEntity.getOldDisableReplies())
            && !Objects.equals(
            postEntity.getDisableReplies(),
            postEntity.getOldDisableReplies()
        )
    ) {
      String key;
      String userKey;
      if (Boolean.TRUE.equals(postEntity.getDisableReplies())) {
        key = "reply.create.disable.message";
        userKey = "reply.create.disable.user.message";
      } else {
        key = "reply.create.disable.cancel.message";
        userKey = "reply.create.disable.cancel.user.message";
      }

      sendDisableCommentReplyMessage(
          key,
          dto.replyDisableReason(),
          postEntity
      );
      sendDisableCommentReplyUserMessage(
          userKey,
          dto.replyDisableReason(),
          postEntity
      );
    }
  }

  /**
   * after advice.
   */
  @After(value = "disableUserCommentReplyPointcut(id,userId,dto)", argNames = "id,userId,dto")
  public void disableUserCommentReplyAfterAdvice(Long id, Long userId, DisableUserCommentReplyPostDto dto) {
    PostEntity postEntity = postRepository.findById(id).orElseThrow(PointNotFoundException::new);
    UserEntity userEntity = userRepository.findById(userId).orElseThrow(UserNotFoundException::new);
    Optional<PostUserEntity> postUserEntityOptional = userEntity.getUserPosts()
        .stream()
        .filter(postUserEntity -> postUserEntity.getPost().equals(postEntity)
            && postUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    if (postUserEntityOptional.isEmpty()) {
      return;
    }

    PostUserEntity postUserEntity = postUserEntityOptional.get();
    if (
        Objects.nonNull(dto.disableComments())
            && Objects.nonNull(postUserEntity.getOldDisableComments())
            && !Objects.equals(
            postUserEntity.getDisableComments(),
            postUserEntity.getOldDisableComments()
        )
    ) {
      String key;
      String userKey;
      if (Boolean.TRUE.equals(postUserEntity.getDisableComments())) {
        key = "user.post.comment.create.disable.message";
        userKey = "user.post.comment.create.disable.user.message";
      } else {
        key = "user.post.comment.create.disable.cancel.message";
        userKey = "user.post.comment.create.disable.cancel.user.message";
      }

      sendUserDisableCommentReplyMessage(
          key,
          dto.commentDisableReason(),
          postUserEntity
      );
      sendUserDisableCommentReplyUserMessage(
          userKey,
          dto.commentDisableReason(),
          postUserEntity
      );
    }

    if (
        Objects.nonNull(dto.disableReplies())
            && Objects.nonNull(postUserEntity.getOldDisableReplies())
            && !Objects.equals(
            postUserEntity.getDisableReplies(),
            postUserEntity.getOldDisableReplies()
        )
    ) {
      String key;
      String userKey;
      if (Boolean.TRUE.equals(postUserEntity.getDisableReplies())) {
        key = "user.post.reply.create.disable.message";
        userKey = "user.post.reply.create.disable.user.message";
      } else {
        key = "user.post.reply.create.disable.cancel.message";
        userKey = "user.post.reply.create.disable.cancel.user.message";
      }

      sendUserDisableCommentReplyMessage(
          key,
          dto.replyDisableReason(),
          postUserEntity
      );
      sendUserDisableCommentReplyUserMessage(
          userKey,
          dto.replyDisableReason(),
          postUserEntity
      );
    }
  }

  /**
   * sendDisableCommentReplyMessage.
   *
   * @param key        key
   * @param reason     reason
   * @param postEntity postEntity
   */
  private void sendDisableCommentReplyMessage(String key, String reason, PostEntity postEntity) {
    if (securityService.isAnonymous()) {
      return;
    }

    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setOverview(
        i18nTool.getMessage(
            key,
            Map.of(
                "name", postEntity.getNameAndId(),
                "alias", securityService.getAliasAndId(postEntity.getUser()),
                "reason", reason,
                "time", getCurrentDateTime()
            )
        )
    );
    messageEntity.setLink(postEntity.getLink());
    messageEntity.setReceiver(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * sendDisableCommentReplyUserMessage.
   *
   * @param key        key
   * @param reason     reason
   * @param postEntity postEntity
   */
  private void sendDisableCommentReplyUserMessage(String key, String reason, PostEntity postEntity) {
    if (Objects.isNull(postEntity.getUser())) {
      return;
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setOverview(i18nTool.getMessage(
        key,
        Map.of(
            "name", postEntity.getNameAndId(),
            "alias", securityService.getAliasAndIdOrNull(),
            "reason", reason,
            "time", getCurrentDateTime()
        )
    ));
    messageEntity.setLink(postEntity.getLink());
    messageEntity.setReceiver(postEntity.getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * sendUserDisableCommentReplyMessage.
   *
   * @param key            key
   * @param reason         reason
   * @param postUserEntity postUserEntity
   */
  private void sendUserDisableCommentReplyMessage(String key, String reason, PostUserEntity postUserEntity) {
    if (securityService.isAnonymous()) {
      return;
    }

    PostEntity postEntity = postUserEntity.getPost();
    UserEntity receiver = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setOverview(
        i18nTool.getMessage(
            key,
            Map.of(
                "name", postEntity.getNameAndId(),
                "alias", securityService.getAliasAndId(postUserEntity.getUser()),
                "reason", reason,
                "time", getCurrentDateTime()
            )
        )
    );
    messageEntity.setLink(postEntity.getLink());
    messageEntity.setReceiver(receiver);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * sendUserDisableCommentReplyUserMessage.
   *
   * @param key            key
   * @param reason         reason
   * @param postUserEntity postUserEntity
   */
  private void sendUserDisableCommentReplyUserMessage(String key, String reason, PostUserEntity postUserEntity) {
    PostEntity postEntity = postUserEntity.getPost();
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setOverview(i18nTool.getMessage(
        key,
        Map.of(
            "name", postEntity.getNameAndId(),
            "alias", securityService.getAliasAndIdOrNull(),
            "reason", reason,
            "time", getCurrentDateTime()
        )
    ));
    messageEntity.setLink(postEntity.getLink());
    messageEntity.setReceiver(postUserEntity.getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }
}
