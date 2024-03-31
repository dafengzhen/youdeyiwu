package com.youdeyiwu.aspect;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import com.youdeyiwu.event.PointPermissionRuleApplicationEvent;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.SectionNotFoundException;
import com.youdeyiwu.model.dto.forum.UpdatePostDto;
import com.youdeyiwu.model.dto.point.PointPermissionRuleEventDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.forum.TagEntity;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.SectionRepository;
import com.youdeyiwu.repository.forum.TagRepository;
import com.youdeyiwu.tool.I18nTool;
import java.util.List;
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

  private final PostRepository postRepository;

  private final TagRepository tagRepository;

  private final SectionRepository sectionRepository;

  private final I18nTool i18nTool;

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
            PermissionRuleNameEnum.CREATE_POST,
            i18nTool.getMessage("point.createPost"),
            null
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
            PermissionRuleNameEnum.CREATE_COMMENT,
            i18nTool.getMessage("point.commentPost"),
            null
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
            PermissionRuleNameEnum.CREATE_REPLY,
            i18nTool.getMessage("point.replyPost"),
            null
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
            PermissionRuleNameEnum.UPDATE_POST,
            i18nTool.getMessage("point.updatePost"),
            null
        )
    ));
  }

  /**
   * before advice.
   */
  @Before(value = "updatePostByIdAndDtoPointcut(id, dto)", argNames = "id,dto")
  public void updatePostByIdAndDtoBeforeAdvice(Long id, UpdatePostDto dto) {
    PostEntity postEntity = postRepository.findById(id)
        .orElseThrow(PostNotFoundException::new);

    if (!CollectionUtils.isEmpty(dto.tags())) {
      List<TagEntity> tags = dto.tags()
          .stream()
          .map(tagRepository::findByName)
          .toList();

      if (postEntity.getTags().containsAll(tags)) {
        return;
      }

      publisher.publishEvent(new PointPermissionRuleApplicationEvent(
          new PointPermissionRuleEventDto(
              PermissionRuleNameEnum.ADD_POST_TAG,
              postEntity.getNameAndId() + "#" + i18nTool.getMessage("point.updatePostTags"),
              postEntity.getLink()
          )
      ));
    }

    if (
        StringUtils.hasText(dto.contentLink())
            && !dto.contentLink().equals(postEntity.getContentLink())
    ) {
      publisher.publishEvent(new PointPermissionRuleApplicationEvent(
          new PointPermissionRuleEventDto(
              PermissionRuleNameEnum.ADD_POST_CONTENT_LINK,
              postEntity.getNameAndId() + "#" + i18nTool.getMessage("point.updatePostContentLink"),
              postEntity.getLink()
          )
      ));
    }

    if (
        StringUtils.hasText(dto.cover())
            && !dto.cover().equals(postEntity.getCover())
    ) {
      publisher.publishEvent(new PointPermissionRuleApplicationEvent(
          new PointPermissionRuleEventDto(
              PermissionRuleNameEnum.ADD_POST_COVER_LINK,
              postEntity.getNameAndId() + "#" + i18nTool.getMessage("point.updatePostCoverLink"),
              postEntity.getLink()
          )
      ));
    }

    if (Objects.nonNull(dto.sectionId())) {
      SectionEntity sectionEntity = sectionRepository.findById(dto.sectionId())
          .orElseThrow(SectionNotFoundException::new);
      if (Objects.equals(postEntity.getSection(), sectionEntity)) {
        return;
      }

      publisher.publishEvent(new PointPermissionRuleApplicationEvent(
          new PointPermissionRuleEventDto(
              PermissionRuleNameEnum.ADD_POST_SECTION,
              postEntity.getNameAndId() + "#" + i18nTool.getMessage("point.updatePostSection"),
              postEntity.getLink()
          )
      ));
    }
  }
}
