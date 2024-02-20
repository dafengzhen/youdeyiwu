package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.security.SecurityService;
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
    final String currentDateTime = getCurrentDateTime();
    PostEntity postEntity = quoteReplyEntity.getPost();
    UserEntity userEntity = quoteReplyEntity.getUser();

    if (Objects.nonNull(postEntity.getUser()) && Objects.nonNull(userEntity)) {
      MessageEntity messageEntity = new MessageEntity();
      messageEntity.setName("You have received a new reply");
      messageEntity.setOverview(
          """
              %s user replied to your article in %s at %s. The reply says: %s.
              """
              .formatted(
                  securityService.getAliasAndId(userEntity),
                  currentDateTime,
                  postEntity.getName(),
                  quoteReplyEntity.getContent()
              )
      );
      Map<String, String> content = messageEntity.getContent();
      content.put("postId", postEntity.getId().toString());
      content.put("sender", userEntity.getId().toString());
      content.put("receiver", postEntity.getUser().getId().toString());
      messageEntity.setLink("/posts/" + postEntity.getId());
      messageEntity.setReceiver(postEntity.getUser());
      publisher.publishEvent(new MessageApplicationEvent(messageEntity));
    }

    if (
        Objects.nonNull(userEntity)
            && !Objects.equals(postEntity.getUser(), userEntity)
    ) {
      MessageEntity messageEntity = new MessageEntity();
      messageEntity.setName("You have received a new reply");
      messageEntity.setOverview(
          """
              %s user replied to your comment in the article %s at %s. The reply says: %s
              """
              .formatted(
                  securityService.getAliasAndId(userEntity),
                  currentDateTime,
                  postEntity.getName(),
                  quoteReplyEntity.getContent()
              )
      );
      Map<String, String> content = messageEntity.getContent();
      content.put("postId", postEntity.getId().toString());

      if (Objects.nonNull(dto.commentId())) {
        content.put("commentId", quoteReplyEntity.getComment().getId().toString());
      }
      if (Objects.nonNull(dto.replyId())) {
        content.put("commentId", quoteReplyEntity.getComment().getId().toString());
        content.put("replyId", quoteReplyEntity.getQuoteReply().getId().toString());
      }

      content.put("sender", userEntity.getId().toString());
      content.put("receiver", userEntity.getId().toString());
      messageEntity.setLink("/posts/" + postEntity.getId());
      messageEntity.setReceiver(userEntity);
      publisher.publishEvent(new MessageApplicationEvent(messageEntity));
    }
  }
}
