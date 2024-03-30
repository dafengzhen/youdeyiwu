package com.youdeyiwu.aspect;

import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.event.PointRuleApplicationEvent;
import com.youdeyiwu.exception.CommentNotFoundException;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.ReplyNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.forum.ApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.CreateCommentDto;
import com.youdeyiwu.model.dto.forum.CreatePostDto;
import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.dto.forum.NotApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.UpdateStatesPostDto;
import com.youdeyiwu.model.dto.point.PointRuleEventDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.CommentUserEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostUserEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyUserEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.forum.CommentRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.ReplyRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

/**
 * point rule.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class PointRuleAspect {

  private final ApplicationEventPublisher publisher;

  private final UserRepository userRepository;

  private final PostRepository postRepository;

  private final SecurityService securityService;

  private final CommentRepository commentRepository;

  private final ReplyRepository replyRepository;

  /**
   * create post.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.create(..)) && args(dto)", argNames = "dto")
  public void createPostPointcut(CreatePostDto dto) {
    // Pointcut
  }

  /**
   * view page.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.viewPage(..)) && args(id, ip)", argNames = "id,ip")
  public void viewPagePointcut(Long id, String ip) {
    // Pointcut
  }

  /**
   * update like.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.updateLike(..)) && args(id)", argNames = "id")
  public void updateLikePointcut(Long id) {
    // Pointcut
  }

  /**
   * update comment like.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.CommentServiceImpl.updateLike(..)) && args(id)", argNames = "id")
  public void updateCommentLikePointcut(Long id) {
    // Pointcut
  }

  /**
   * update reply like.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.ReplyServiceImpl.updateLike(..)) && args(id)", argNames = "id")
  public void updateReplyLikePointcut(Long id) {
    // Pointcut
  }

  /**
   * update favorite.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.updateFavorite(..)) && args(id)", argNames = "id")
  public void updateFavoritePointcut(Long id) {
    // Pointcut
  }

  /**
   * update states.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostServiceImpl.updateStates(..)) && args(id, dto)", argNames = "id,dto")
  public void updateStatesPointcut(Long id, UpdateStatesPostDto dto) {
    // Pointcut
  }

  /**
   * post review approved.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostReviewQueueServiceImpl.approved(..)) && args(id, dto)", argNames = "id,dto")
  public void postReviewApprovedPointcut(Long id, ApprovedPostReviewQueueDto dto) {
    // Pointcut
  }

  /**
   * post review not approved.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostReviewQueueServiceImpl.notApproved(..)) && args(id, dto)", argNames = "id,dto")
  public void postReviewNotApprovedPointcut(Long id, NotApprovedPostReviewQueueDto dto) {
    // Pointcut
  }

  /**
   * create comment.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.CommentServiceImpl.create(..)) && args(dto)", argNames = "dto")
  public void createCommentPointcut(CreateCommentDto dto) {
    // Pointcut
  }

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
  @AfterReturning(value = "createPostPointcut(dto)", returning = "postEntity", argNames = "postEntity,dto")
  public void createPostAfterReturningAdvice(PostEntity postEntity, CreatePostDto dto) {
    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.CREATE_POST,
            SignEnum.POSITIVE,
            true,
            """
                %s [ %s ]
                """
                .formatted(postEntity.getName(), "Create Post"),
            "/posts/" + postEntity.getId(),
            null,
            null,
            false
        )
    ));
  }

  /**
   * after advice.
   */
  @After(value = "viewPagePointcut(id,ip)", argNames = "id,ip")
  public void viewPageAfterAdvice(Long id, String ip) {
    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.VISIT_POST,
            SignEnum.POSITIVE,
            false,
            null,
            null,
            id,
            null,
            false
        )
    ));
  }

  /**
   * after advice.
   */
  @After(value = "updateLikePointcut(id)", argNames = "id")
  public void updateLikeAfterAdvice(Long id) {
    UserEntity userEntity = userRepository
        .findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    PostEntity postEntity = postRepository.findById(id)
        .orElseThrow(PostNotFoundException::new);
    Optional<PostUserEntity> postUserEntityOptional = userEntity.getUserPosts()
        .stream()
        .filter(postUserEntity -> postUserEntity.getPost().equals(postEntity)
            && postUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    Boolean liked = postUserEntityOptional
        .map(postUserEntity -> !postUserEntity.getLiked())
        .orElse(true);

    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.LIKE_POST,
            Boolean.TRUE.equals(liked)
                ? SignEnum.NEGATIVE
                : SignEnum.POSITIVE,
            false,
            null,
            null,
            id,
            null,
            true
        )
    ));
  }

  /**
   * after advice.
   */
  @After(value = "updateCommentLikePointcut(id)", argNames = "id")
  public void updateCommentLikeAfterAdvice(Long id) {
    UserEntity userEntity = userRepository
        .findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    CommentEntity commentEntity = commentRepository.findById(id)
        .orElseThrow(CommentNotFoundException::new);
    Optional<CommentUserEntity> commentUserEntityOptional = userEntity.getUserComments()
        .stream()
        .filter(commentUserEntity -> commentUserEntity.getComment().equals(commentEntity)
            && commentUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    Boolean liked = commentUserEntityOptional
        .map(commentUserEntity -> !commentUserEntity.getLiked())
        .orElse(true);

    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.LIKE_COMMENT,
            Boolean.TRUE.equals(liked)
                ? SignEnum.NEGATIVE
                : SignEnum.POSITIVE,
            false,
            null,
            null,
            commentEntity.getPost().getId(),
            null,
            true
        )
    ));
  }

  /**
   * after advice.
   */
  @After(value = "updateReplyLikePointcut(id)", argNames = "id")
  public void updateReplyLikeAfterAdvice(Long id) {
    UserEntity userEntity = userRepository
        .findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    QuoteReplyEntity quoteReplyEntity = replyRepository.findById(id)
        .orElseThrow(ReplyNotFoundException::new);
    Optional<QuoteReplyUserEntity> quoteReplyUserEntityOptional = userEntity.getUserQuoteReplies()
        .stream()
        .filter(quoteReplyUserEntity -> quoteReplyUserEntity.getQuoteReply().equals(quoteReplyEntity)
            && quoteReplyUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    Boolean liked = quoteReplyUserEntityOptional
        .map(commentUserEntity -> !commentUserEntity.getLiked())
        .orElse(true);

    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.LIKE_REPLY,
            Boolean.TRUE.equals(liked)
                ? SignEnum.NEGATIVE
                : SignEnum.POSITIVE,
            false,
            null,
            null,
            quoteReplyEntity.getPost().getId(),
            null,
            true
        )
    ));
  }

  /**
   * after advice.
   */
  @After(value = "updateFavoritePointcut(id)", argNames = "id")
  public void updateFavoriteAfterAdvice(Long id) {
    UserEntity userEntity = userRepository
        .findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    PostEntity postEntity = postRepository.findById(id)
        .orElseThrow(PostNotFoundException::new);
    Optional<PostUserEntity> postUserEntityOptional = userEntity.getUserPosts()
        .stream()
        .filter(postUserEntity -> postUserEntity.getPost().equals(postEntity)
            && postUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    Boolean favorited = postUserEntityOptional
        .map(postUserEntity -> !postUserEntity.getFavorited())
        .orElse(true);

    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.FAVORITE_POST,
            Boolean.TRUE.equals(favorited)
                ? SignEnum.NEGATIVE
                : SignEnum.POSITIVE,
            false,
            null,
            null,
            id,
            null,
            true
        )
    ));
  }

  /**
   * after advice.
   */
  @After(value = "updateStatesPointcut(id,dto)", argNames = "id,dto")
  public void updateStatesAfterAdvice(Long id, UpdateStatesPostDto dto) {
    PostEntity postEntity = postRepository.findById(id)
        .orElseThrow(PostNotFoundException::new);

    if (
        Objects.nonNull(dto.reviewState())
            && dto.reviewState() != postEntity.getReviewState()
    ) {
      RuleNameEnum ruleName;
      SignEnum sign = switch (dto.reviewState()) {
        case APPROVED -> {
          ruleName = RuleNameEnum.POST_APPROVED;
          yield SignEnum.POSITIVE;
        }
        case REJECTED -> {
          ruleName = RuleNameEnum.POST_NOT_APPROVED;
          yield SignEnum.NEGATIVE;
        }
        case PENDING_REVIEW -> {
          ruleName = RuleNameEnum.POST_PENDING_REVIEW;
          yield SignEnum.NEGATIVE;
        }
        default -> throw new IllegalStateException("Unexpected value: " + dto.reviewState());
      };

      publisher.publishEvent(new PointRuleApplicationEvent(
          new PointRuleEventDto(
              ruleName,
              sign,
              false,
              null,
              null,
              id,
              null,
              false
          )
      ));
    }
  }

  /**
   * after advice.
   */
  @After(value = "postReviewApprovedPointcut(id,dto)", argNames = "id,dto")
  public void postReviewApprovedAfterAdvice(Long id, ApprovedPostReviewQueueDto dto) {
    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.POST_APPROVED,
            SignEnum.POSITIVE,
            false,
            null,
            null,
            id,
            null,
            false
        )
    ));
  }

  /**
   * after advice.
   */
  @After(value = "postReviewNotApprovedPointcut(id,dto)", argNames = "id,dto")
  public void postReviewNotApprovedAfterAdvice(Long id, NotApprovedPostReviewQueueDto dto) {
    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.POST_NOT_APPROVED,
            SignEnum.NEGATIVE,
            false,
            null,
            null,
            id,
            null,
            false
        )
    ));
  }

  /**
   * after returning advice.
   */
  @AfterReturning(value = "createCommentPointcut(dto)", returning = "commentEntity", argNames = "commentEntity,dto")
  public void createCommentAfterReturningAdvice(CommentEntity commentEntity, CreateCommentDto dto) {
    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.COMMENT_POST,
            SignEnum.POSITIVE,
            false,
            null,
            null,
            commentEntity.getPost().getId(),
            null,
            false
        )
    ));
  }

  /**
   * after returning advice.
   */
  @AfterReturning(value = "createReplyPointcut(dto)", returning = "quoteReplyEntity", argNames = "quoteReplyEntity,dto")
  public void createReplyAfterReturningAdvice(
      QuoteReplyEntity quoteReplyEntity,
      CreateReplyDto dto
  ) {
    Optional<UserEntity> commentOrReplyUserEntity = Optional.empty();
    if (Objects.nonNull(quoteReplyEntity.getComment())) {
      commentOrReplyUserEntity = Optional.ofNullable(quoteReplyEntity.getComment().getUser());
    }
    if (Objects.nonNull(quoteReplyEntity.getQuoteReply())) {
      commentOrReplyUserEntity = Optional.ofNullable(quoteReplyEntity.getQuoteReply().getUser());
    }

    Set<Long> receiverUserIds;
    if (securityService.isAnonymous()) {
      receiverUserIds = null;
    } else if (
        Objects.equals(
            userRepository.findById(securityService.getUserId()).orElseThrow(UserNotFoundException::new),
            commentOrReplyUserEntity.orElse(null)
        )
    ) {
      receiverUserIds = null;
    } else {
      receiverUserIds = commentOrReplyUserEntity.map(user -> Set.of(user.getId())).orElse(null);
    }

    publisher.publishEvent(new PointRuleApplicationEvent(
        new PointRuleEventDto(
            RuleNameEnum.REPLY_POST,
            SignEnum.POSITIVE,
            false,
            null,
            null,
            quoteReplyEntity.getPost().getId(),
            receiverUserIds,
            false
        )
    ));
  }
}
