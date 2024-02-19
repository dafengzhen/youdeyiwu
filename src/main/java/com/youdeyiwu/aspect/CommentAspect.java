package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.model.dto.forum.CreateCommentDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
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
    UserEntity userEntity = commentEntity.getUser();
    if (Objects.isNull(postEntity.getUser()) || Objects.isNull(userEntity)) {
      return;
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("You have received a new comment");
    messageEntity.setOverview(
        """
            %s user commented on your article in %s at %s. The comment says: %s.
            """
            .formatted(
                securityService.getAliasAndId(userEntity),
                getCurrentDateTime(),
                postEntity.getName(),
                commentEntity.getContent()
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
}
