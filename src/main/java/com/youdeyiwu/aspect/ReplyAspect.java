package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.tool.I18nTool;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

/**
 * reply.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class ReplyAspect {

  private final ApplicationEventPublisher publisher;

  private final SecurityService securityService;

  private final I18nTool i18nTool;

  /**
   * create reply.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.ReplyServiceImpl.create(..)) && args(dto)", argNames = "dto")
  public void createReplyPointcut(CreateReplyDto dto) {
    // Pointcut
  }

  /**
   * after returning advice.
   */
  @AfterReturning(value = "createReplyPointcut(dto)", returning = "quoteReplyEntity", argNames = "quoteReplyEntity,dto")
  public void createReplyAfterReturningAdvice(
      QuoteReplyEntity quoteReplyEntity,
      CreateReplyDto dto
  ) {
    sendToPostCreator(quoteReplyEntity);
    sendToCommentCreator(quoteReplyEntity);
    sendToReplyCreator(quoteReplyEntity);
  }

  /**
   * send to post creator.
   *
   * @param quoteReplyEntity quoteReplyEntity
   */
  private void sendToPostCreator(QuoteReplyEntity quoteReplyEntity) {
    PostEntity postEntity = quoteReplyEntity.getPost();
    if (Objects.isNull(postEntity.getUser())) {
      return;
    }

    MessageEntity messageEntity = createMessageEntity(quoteReplyEntity);
    messageEntity.setReceiver(postEntity.getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send to comment creator.
   *
   * @param quoteReplyEntity quoteReplyEntity
   */
  private void sendToCommentCreator(QuoteReplyEntity quoteReplyEntity) {
    CommentEntity commentEntity = quoteReplyEntity.getComment();
    if (Objects.isNull(commentEntity) || Objects.isNull(commentEntity.getUser())) {
      return;
    }

    MessageEntity messageEntity = createMessageEntity(quoteReplyEntity);
    messageEntity.setReceiver(commentEntity.getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send to reply creator.
   *
   * @param quoteReplyEntity quoteReplyEntity
   */
  private void sendToReplyCreator(QuoteReplyEntity quoteReplyEntity) {
    QuoteReplyEntity quoteReply = quoteReplyEntity.getQuoteReply();
    if (Objects.isNull(quoteReply) || Objects.isNull(quoteReply.getUser())) {
      return;
    }

    MessageEntity messageEntity = createMessageEntity(quoteReplyEntity);
    messageEntity.setReceiver(quoteReply.getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * create message entity.
   *
   * @param quoteReplyEntity quoteReplyEntity
   * @return MessageEntity
   */
  private MessageEntity createMessageEntity(QuoteReplyEntity quoteReplyEntity) {
    PostEntity postEntity = quoteReplyEntity.getPost();
    Map<String, Object> overviewArgs = new HashMap<>();
    overviewArgs.put("name", postEntity.getNameAndId());
    overviewArgs.put("content", quoteReplyEntity.getContentAndId());
    overviewArgs.put("time", getCurrentDateTime());

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(i18nTool.getMessage("reply.create.message.name"));

    if (Objects.isNull(quoteReplyEntity.getUser())) {
      messageEntity.setOverview(i18nTool.getMessage("reply.create.message.overview.anonymous", overviewArgs));
    } else {
      overviewArgs.put("alias", securityService.getAliasAndId(quoteReplyEntity.getUser()));
      messageEntity.setOverview(i18nTool.getMessage("reply.create.message.overview", overviewArgs));
    }

    messageEntity.setLink(postEntity.getLink());
    return messageEntity;
  }
}
