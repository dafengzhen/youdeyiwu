package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PostReviewStateApplicationEvent;
import com.youdeyiwu.exception.PointNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.forum.DisableCommentReplyPostDto;
import com.youdeyiwu.model.dto.forum.UpdateStatesPostDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.tool.I18nTool;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
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
}
