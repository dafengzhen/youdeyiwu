package com.youdeyiwu.aspect;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import com.youdeyiwu.event.PointPermissionRuleApplicationEvent;
import com.youdeyiwu.model.dto.forum.UpdatePostDto;
import com.youdeyiwu.model.dto.point.PointPermissionRuleEventDto;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * point permission rule.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class PointPermissionRuleAspect {

  private final ApplicationEventPublisher publisher;

  /**
   * create post.
   */
  @Pointcut("execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.create(..))")
  public void createPostPointcut() {
    // Pointcut
  }

  /**
   * create comment.
   */
  @Pointcut("execution(* com.youdeyiwu.service.forum.impl.CommentServiceImpl.create(..))")
  public void createCommentPointcut() {
    // Pointcut
  }

  /**
   * create reply.
   */
  @Pointcut("execution(* com.youdeyiwu.service.forum.impl.ReplyServiceImpl.create(..))")
  public void createReplyPointcut() {
    // Pointcut
  }

  /**
   * update post.
   */
  @Pointcut("execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.update(..))")
  public void updatePostPointcut() {
    // Pointcut
  }

  /**
   * update post by id and dto.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.update(..)) && args(id, dto)", argNames = "id,dto")
  public void updatePostByIdAndDtoPointcut(Long id, UpdatePostDto dto) {
    // Pointcut
  }

  /**
   * before advice.
   */
  @Before("createPostPointcut()")
  public void createPostBeforeAdvice() {
    publisher.publishEvent(new PointPermissionRuleApplicationEvent(
        new PointPermissionRuleEventDto(
            PermissionRuleNameEnum.CREATE_POST
        )
    ));
  }

  /**
   * before advice.
   */
  @Before("createCommentPointcut()")
  public void createCommentBeforeAdvice() {
    publisher.publishEvent(new PointPermissionRuleApplicationEvent(
        new PointPermissionRuleEventDto(
            PermissionRuleNameEnum.CREATE_COMMENT
        )
    ));
  }

  /**
   * before advice.
   */
  @Before("createReplyPointcut()")
  public void createReplyBeforeAdvice() {
    publisher.publishEvent(new PointPermissionRuleApplicationEvent(
        new PointPermissionRuleEventDto(
            PermissionRuleNameEnum.CREATE_REPLY
        )
    ));
  }

  /**
   * before advice.
   */
  @Before("updatePostPointcut()")
  public void updatePostBeforeAdvice() {
    publisher.publishEvent(new PointPermissionRuleApplicationEvent(
        new PointPermissionRuleEventDto(
            PermissionRuleNameEnum.UPDATE_POST
        )
    ));
  }

  /**
   * before advice.
   */
  @Before(value = "updatePostByIdAndDtoPointcut(id, dto)", argNames = "id,dto")
  public void updatePostByIdAndDtoBeforeAdvice(Long id, UpdatePostDto dto) {
    if (!CollectionUtils.isEmpty(dto.tags())) {
      publisher.publishEvent(new PointPermissionRuleApplicationEvent(
          new PointPermissionRuleEventDto(
              PermissionRuleNameEnum.ADD_POST_TAG
          )
      ));
    }

    if (StringUtils.hasText(dto.contentLink())) {
      publisher.publishEvent(new PointPermissionRuleApplicationEvent(
          new PointPermissionRuleEventDto(
              PermissionRuleNameEnum.ADD_POST_CONTENT_LINK
          )
      ));
    }

    if (StringUtils.hasText(dto.cover())) {
      publisher.publishEvent(new PointPermissionRuleApplicationEvent(
          new PointPermissionRuleEventDto(
              PermissionRuleNameEnum.ADD_POST_COVER_LINK
          )
      ));
    }

    if (Objects.nonNull(dto.sectionId())) {
      publisher.publishEvent(new PointPermissionRuleApplicationEvent(
          new PointPermissionRuleEventDto(
              PermissionRuleNameEnum.ADD_POST_SECTION
          )
      ));
    }
  }
}
