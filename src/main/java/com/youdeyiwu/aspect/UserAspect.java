package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.user.DisableCommentReplyUserDto;
import com.youdeyiwu.model.dto.user.LoginDto;
import com.youdeyiwu.model.dto.user.RegisterDto;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.TokenVo;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.tool.I18nTool;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

/**
 * user.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class UserAspect {

  private final ApplicationEventPublisher publisher;

  private final UserRepository userRepository;

  private final I18nTool i18nTool;

  private final SecurityService securityService;

  /**
   * register.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.user.impl.UserServiceImpl.register(..)) && args(dto)", argNames = "dto")
  public void registerPointcut(RegisterDto dto) {
    // Pointcut
  }

  /**
   * login.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.user.impl.UserServiceImpl.login(..)) && args(dto)", argNames = "dto")
  public void loginPointcut(LoginDto dto) {
    // Pointcut
  }

  /**
   * disableCommentReply.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.user.impl.UserServiceImpl.disableCommentReply(..)) && args(id,dto)", argNames = "id,dto")
  public void disableCommentReplyPointcut(Long id, DisableCommentReplyUserDto dto) {
    // Pointcut
  }

  /**
   * after returning advice.
   */
  @AfterReturning(value = "registerPointcut(dto)", returning = "vo", argNames = "vo,dto")
  public void registerAfterReturningAdvice(TokenVo vo, RegisterDto dto) {
    UserEntity userEntity = userRepository.getReferenceById(vo.getId());
    MessageEntity messageEntity = createMessageEntity(
        userEntity,
        "user.register.message.name",
        "user.register.message.overview"
    );
    messageEntity.setSender(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * after returning advice.
   */
  @AfterReturning(value = "loginPointcut(dto)", returning = "vo", argNames = "vo,dto")
  public void loginAfterReturningAdvice(TokenVo vo, LoginDto dto) {
    UserEntity userEntity = userRepository.getReferenceById(vo.getId());
    MessageEntity messageEntity = createMessageEntity(
        userEntity,
        "user.login.message.name",
        "user.login.message.overview"
    );
    messageEntity.setSender(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * after advice.
   */
  @After(value = "disableCommentReplyPointcut(id,dto)", argNames = "id,dto")
  public void disableCommentReplyAfterAdvice(Long id, DisableCommentReplyUserDto dto) {
    UserEntity userEntity = userRepository.findById(id)
        .orElseThrow(UserNotFoundException::new);

    if (
        Objects.nonNull(dto.noPostingAllowed())
            && Objects.nonNull(userEntity.getOldNoPostingAllowed())
            && !Objects.equals(
            userEntity.getNoPostingAllowed(),
            userEntity.getOldNoPostingAllowed()
        )
    ) {
      String key;
      String userKey;
      if (Boolean.TRUE.equals(userEntity.getNoPostingAllowed())) {
        key = "user.noPostingAllowed.true.message";
        userKey = "user.noPostingAllowed.true.user.message";
      } else {
        key = "user.noPostingAllowed.false.message";
        userKey = "user.noPostingAllowed.false.user.message";
      }

      sendDisableCommentReplyMessage(
          key,
          dto.noPostingReason(),
          userEntity
      );
      sendDisableCommentReplyUserMessage(
          userKey,
          dto.noPostingReason(),
          userEntity
      );
    }

    if (
        Objects.nonNull(dto.disableComments())
            && Objects.nonNull(userEntity.getOldDisableComments())
            && !Objects.equals(
            userEntity.getDisableComments(),
            userEntity.getOldDisableComments()
        )
    ) {
      String key;
      String userKey;
      if (Boolean.TRUE.equals(userEntity.getDisableComments())) {
        key = "user.comment.create.disable.message";
        userKey = "user.comment.create.disable.user.message";
      } else {
        key = "user.comment.create.disable.cancel.message";
        userKey = "user.comment.create.disable.cancel.user.message";
      }

      sendDisableCommentReplyMessage(
          key,
          dto.commentDisableReason(),
          userEntity
      );
      sendDisableCommentReplyUserMessage(
          userKey,
          dto.commentDisableReason(),
          userEntity
      );
    }

    if (
        Objects.nonNull(dto.disableReplies())
            && Objects.nonNull(userEntity.getOldDisableReplies())
            && !Objects.equals(
            userEntity.getDisableReplies(),
            userEntity.getOldDisableReplies()
        )
    ) {
      String key;
      String userKey;
      if (Boolean.TRUE.equals(userEntity.getDisableReplies())) {
        key = "user.reply.create.disable.message";
        userKey = "user.reply.create.disable.user.message";
      } else {
        key = "user.reply.create.disable.cancel.message";
        userKey = "user.reply.create.disable.cancel.user.message";
      }

      sendDisableCommentReplyMessage(
          key,
          dto.replyDisableReason(),
          userEntity
      );
      sendDisableCommentReplyUserMessage(
          userKey,
          dto.replyDisableReason(),
          userEntity
      );
    }
  }

  /**
   * sendDisableCommentReplyMessage.
   *
   * @param key        key
   * @param reason     reason
   * @param userEntity userEntity
   */
  private void sendDisableCommentReplyMessage(String key, String reason, UserEntity userEntity) {
    if (securityService.isAnonymous()) {
      return;
    }

    UserEntity receiver = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setOverview(
        i18nTool.getMessage(
            key,
            Map.of(
                "alias", securityService.getAliasAndId(userEntity),
                "reason", reason,
                "time", getCurrentDateTime()
            )
        )
    );
    messageEntity.setLink(userEntity.getLink());
    messageEntity.setReceiver(receiver);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * sendDisableCommentReplyUserMessage.
   *
   * @param key        key
   * @param reason     reason
   * @param userEntity userEntity
   */
  private void sendDisableCommentReplyUserMessage(String key, String reason, UserEntity userEntity) {
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setOverview(i18nTool.getMessage(
        key,
        Map.of(
            "alias", securityService.getAliasAndIdOrNull(),
            "reason", reason,
            "time", getCurrentDateTime()
        )
    ));
    messageEntity.setLink(userEntity.getLink());
    messageEntity.setReceiver(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * create message entity.
   *
   * @param userEntity       userEntity
   * @param eventNameKey     eventNameKey
   * @param eventOverviewKey eventOverviewKey
   * @return MessageEntity
   */
  private MessageEntity createMessageEntity(UserEntity userEntity, String eventNameKey, String eventOverviewKey) {
    Map<String, Object> overviewArgs = new HashMap<>();
    overviewArgs.put("time", userEntity.getId() + "#" + getCurrentDateTime());

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(i18nTool.getMessage(eventNameKey));
    messageEntity.setOverview(i18nTool.getMessage(eventOverviewKey, overviewArgs));
    messageEntity.setLink(userEntity.getLink());
    messageEntity.setReceiver(userEntity);
    return messageEntity;
  }
}
