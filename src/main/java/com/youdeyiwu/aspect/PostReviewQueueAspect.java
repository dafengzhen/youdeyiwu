package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.PostReviewQueueNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.forum.ApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.NotApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.ReceivePostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.RefundPostReviewQueueDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostReviewQueueEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.PostReviewQueueRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

/**
 * post review queue.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class PostReviewQueueAspect {

  private final PostReviewQueueRepository postReviewQueueRepository;

  private final ApplicationEventPublisher publisher;

  private final UserRepository userRepository;

  private final PostRepository postRepository;

  private final SecurityService securityService;

  /**
   * receive.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostReviewQueueServiceImpl.receive(..)) && args(dto)", argNames = "dto")
  public void receivePointcut(ReceivePostReviewQueueDto dto) {
    // Pointcut
  }

  /**
   * refund.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostReviewQueueServiceImpl.refund(..)) && args(id,dto)", argNames = "id,dto")
  public void refundPointcut(Long id, RefundPostReviewQueueDto dto) {
    // Pointcut
  }

  /**
   * approved.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostReviewQueueServiceImpl.approved(..)) && args(id,dto)", argNames = "id,dto")
  public void approvedPointcut(Long id, ApprovedPostReviewQueueDto dto) {
    // Pointcut
  }

  /**
   * notApproved.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostReviewQueueServiceImpl.notApproved(..)) && args(id,dto)", argNames = "id,dto")
  public void notApprovedPointcut(Long id, NotApprovedPostReviewQueueDto dto) {
    // Pointcut
  }

  /**
   * after advice.
   */
  @After(value = "receivePointcut(dto)", argNames = "dto")
  public void receiveAfterAdvice(ReceivePostReviewQueueDto dto) {
    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    Optional<PostReviewQueueEntity> postReviewQueueEntityOptional = Optional.empty();

    if (postReviewQueueRepository.existsByPost(postEntity)) {
      PostReviewQueueEntity entity = postReviewQueueRepository.findByPost(postEntity);
      if (Boolean.FALSE.equals(entity.getReceived())) {
        postReviewQueueEntityOptional = Optional.of(entity);
      }
    } else {
      PostReviewQueueEntity entity = new PostReviewQueueEntity();
      entity.setReceiver(userEntity);
      entity.setPost(postEntity);
      postReviewQueueEntityOptional = Optional.of(entity);
    }

    if (postReviewQueueEntityOptional.isEmpty()) {
      return;
    }

    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueEntityOptional.get();
    String currentDateTime = getCurrentDateTime();
    String estimatedTime = getCurrentDateTime(dto.latestReviewResultTime());
    sendReceivedMessageToReviewer(postReviewQueueEntity, currentDateTime, estimatedTime);
    sendReceivedMessageToUser(postReviewQueueEntity, currentDateTime, estimatedTime);
  }

  /**
   * after advice.
   */
  @After(value = "refundPointcut(id,dto)", argNames = "id,dto")
  public void refundAfterAdvice(Long id, RefundPostReviewQueueDto dto) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);

    String currentDateTime = getCurrentDateTime();
    sendRefundMessageToReviewer(postReviewQueueEntity, currentDateTime);
    sendRefundMessageToUser(postReviewQueueEntity, currentDateTime);
  }

  /**
   * after advice.
   */
  @After(value = "approvedPointcut(id,dto)", argNames = "id,dto")
  public void approvedAfterAdvice(Long id, ApprovedPostReviewQueueDto dto) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    sendApprovedMessageToUser(postReviewQueueEntity, getCurrentDateTime(), dto.reason());
  }

  /**
   * after advice.
   */
  @After(value = "notApprovedPointcut(id,dto)", argNames = "id,dto")
  public void notApprovedAfterAdvice(Long id, NotApprovedPostReviewQueueDto dto) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    sendNotApprovedMessageToUser(postReviewQueueEntity, getCurrentDateTime(), dto.reason());
  }

  /**
   * send received message to reviewer.
   *
   * @param postReviewQueueEntity postReviewQueueEntity
   * @param currentDateTime       currentDateTime
   * @param estimatedTime         estimatedTime
   */
  private void sendReceivedMessageToReviewer(
      PostReviewQueueEntity postReviewQueueEntity,
      String currentDateTime,
      String estimatedTime
  ) {
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("You have received a post review request");

    if (Objects.isNull(postReviewQueueEntity.getPost().getUser())) {
      messageEntity.setOverview(
          """
              At %s, you have claimed the review request for the post [ %s ],
              with an estimated completion time of %s. Thank you for your support to the forum.
              """
              .formatted(
                  currentDateTime,
                  postReviewQueueEntity.getPost().getName(),
                  estimatedTime
              )
      );
    } else {
      messageEntity.setOverview(
          """
              You have received a post review request from user %s for the post [ %s ] at %s.
              The estimated completion time for the review is %s.
              Thank you for your support to the forum.
              """
              .formatted(
                  securityService.getAliasAndId(postReviewQueueEntity.getPost().getUser()),
                  postReviewQueueEntity.getPost().getName(),
                  currentDateTime,
                  estimatedTime
              )
      );
    }

    messageEntity.setLink("/posts/" + postReviewQueueEntity.getPost().getId());
    messageEntity.setReceiver(postReviewQueueEntity.getReceiver());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send received message to user.
   *
   * @param postReviewQueueEntity postReviewQueueEntity
   * @param currentDateTime       currentDateTime
   * @param estimatedTime         estimatedTime
   */
  private void sendReceivedMessageToUser(
      PostReviewQueueEntity postReviewQueueEntity,
      String currentDateTime,
      String estimatedTime
  ) {
    if (Objects.isNull(postReviewQueueEntity.getPost().getUser())) {
      return;
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("Your post is under review");
    messageEntity.setOverview(
        """
            Your post [ %s ] will be reviewed by user %s starting at %s.
            The estimated completion time for the review is %s.
            We value every post you make and appreciate your support.
            """
            .formatted(
                postReviewQueueEntity.getPost().getName(),
                securityService.getAliasAndId(postReviewQueueEntity.getReceiver()),
                currentDateTime,
                estimatedTime
            )
    );

    messageEntity.setLink("/posts/" + postReviewQueueEntity.getPost().getId());
    messageEntity.setReceiver(postReviewQueueEntity.getPost().getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send refund message to reviewer.
   *
   * @param postReviewQueueEntity postReviewQueueEntity
   * @param currentDateTime       currentDateTime
   */
  private void sendRefundMessageToReviewer(
      PostReviewQueueEntity postReviewQueueEntity,
      String currentDateTime
  ) {
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("You have returned a post review request");

    if (Objects.isNull(postReviewQueueEntity.getPost().getUser())) {
      messageEntity.setOverview(
          """
              You have returned the review request for post [ %s ] at %s. The review task for this post is now available for other people to claim and process.
              """
              .formatted(
                  postReviewQueueEntity.getPost().getName(),
                  currentDateTime
              )
      );
    } else {
      messageEntity.setOverview(
          """
              You have returned the review request for the post [ %s ] from user %s at %s. The post is now available for other people to claim for review.
              """
              .formatted(
                  postReviewQueueEntity.getPost().getName(),
                  securityService.getAliasAndId(postReviewQueueEntity.getPost().getUser()),
                  currentDateTime
              )
      );
    }

    messageEntity.setLink("/posts/" + postReviewQueueEntity.getPost().getId());
    messageEntity.setReceiver(postReviewQueueEntity.getReceiver());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send refund message to user.
   *
   * @param postReviewQueueEntity postReviewQueueEntity
   * @param currentDateTime       currentDateTime
   */
  private void sendRefundMessageToUser(
      PostReviewQueueEntity postReviewQueueEntity,
      String currentDateTime
  ) {
    if (Objects.isNull(postReviewQueueEntity.getPost().getUser())) {
      return;
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("Your post is currently awaiting review");
    messageEntity.setOverview(
        """
            User %s has released the review request for your post [ %s ] at %s. Your post is now available for other people to review.
            """
            .formatted(
                securityService.getAliasAndId(postReviewQueueEntity.getReceiver()),
                postReviewQueueEntity.getPost().getName(),
                currentDateTime
            )
    );

    messageEntity.setLink("/posts/" + postReviewQueueEntity.getPost().getId());
    messageEntity.setReceiver(postReviewQueueEntity.getPost().getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send approved message to user.
   *
   * @param postReviewQueueEntity postReviewQueueEntity
   * @param currentDateTime       currentDateTime
   * @param reason                reason
   */
  private void sendApprovedMessageToUser(
      PostReviewQueueEntity postReviewQueueEntity,
      String currentDateTime,
      String reason
  ) {
    if (Objects.isNull(postReviewQueueEntity.getPost().getUser())) {
      return;
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("Congratulations, your post has been approved");
    messageEntity.setOverview(
        """
            The user %s, on %s, has approved your post [%s].
            The reason for approval is as follows: %s.
            Your post is now publicly accessible. Congratulations and thank you for your continued support. Every article you write deserves praise.
            """
            .formatted(
                securityService.getAliasAndId(postReviewQueueEntity.getReceiver()),
                currentDateTime,
                postReviewQueueEntity.getPost().getName(),
                StringUtils.hasText(reason) ? reason : "-"
            )
    );

    messageEntity.setLink("/posts/" + postReviewQueueEntity.getPost().getId());
    messageEntity.setReceiver(postReviewQueueEntity.getPost().getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send not approved message to user.
   *
   * @param postReviewQueueEntity postReviewQueueEntity
   * @param currentDateTime       currentDateTime
   * @param reason                reason
   */
  private void sendNotApprovedMessageToUser(
      PostReviewQueueEntity postReviewQueueEntity,
      String currentDateTime,
      String reason
  ) {
    if (Objects.isNull(postReviewQueueEntity.getPost().getUser())) {
      return;
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("Apologies, your post did not pass the review");
    messageEntity.setOverview(
        """
            The user %s, on %s, has determined that your post [%s] cannot be approved.
            The reason for disapproval is as follows: %s.
            Your post will now be restricted from access.
            Please don't be discouraged; you can resubmit your application after revising the article.
            We will continue to accompany you on the forum.
            """
            .formatted(
                securityService.getAliasAndId(postReviewQueueEntity.getReceiver()),
                currentDateTime,
                postReviewQueueEntity.getPost().getName(),
                StringUtils.hasText(reason) ? reason : "-"
            )
    );

    messageEntity.setLink("/posts/" + postReviewQueueEntity.getPost().getId());
    messageEntity.setReceiver(postReviewQueueEntity.getPost().getUser());
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }
}
