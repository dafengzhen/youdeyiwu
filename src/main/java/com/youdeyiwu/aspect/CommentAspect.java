package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.model.dto.forum.CreateCommentDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
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
 * comment.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class CommentAspect {

  private final ApplicationEventPublisher publisher;

  private final SecurityService securityService;

  private final I18nTool i18nTool;

  /**
   * create comment.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.CommentServiceImpl.create(..)) && args(dto)", argNames = "dto")
  public void createCommentPointcut(CreateCommentDto dto) {
    // Pointcut
  }

  /**
   * after returning advice.
   */
  @AfterReturning(value = "createCommentPointcut(dto)", returning = "commentEntity", argNames = "commentEntity,dto")
  public void createCommentAfterReturningAdvice(CommentEntity commentEntity, CreateCommentDto dto) {
    PostEntity postEntity = commentEntity.getPost();
    if (Objects.isNull(postEntity.getUser())) {
      return;
    }

    MessageEntity messageEntity = createMessageEntity(commentEntity);
    messageEntity.setReceiver(postEntity.getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * create message entity.
   *
   * @param commentEntity commentEntity
   * @return MessageEntity
   */
  private MessageEntity createMessageEntity(CommentEntity commentEntity) {
    PostEntity postEntity = commentEntity.getPost();
    Map<String, Object> overviewArgs = new HashMap<>();
    overviewArgs.put("name", postEntity.getNameAndId());
    overviewArgs.put("content", commentEntity.getContentAndId());
    overviewArgs.put("time", getCurrentDateTime());

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(i18nTool.getMessage("comment.create.message.name"));

    if (Objects.isNull(commentEntity.getUser())) {
      messageEntity.setOverview(i18nTool.getMessage("comment.create.message.overview.anonymous", overviewArgs));
    } else {
      overviewArgs.put("alias", securityService.getAliasAndId(commentEntity.getUser()));
      messageEntity.setOverview(i18nTool.getMessage("comment.create.message.overview", overviewArgs));
    }

    messageEntity.setLink(postEntity.getLink());
    return messageEntity;
  }
}
