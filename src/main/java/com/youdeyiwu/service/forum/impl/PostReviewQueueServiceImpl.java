package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.enums.forum.PostReviewStateEnum;
import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PointAutoRuleApplicationEvent;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.PostReviewQueueNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.PostMapper;
import com.youdeyiwu.mapper.forum.PostReviewQueueMapper;
import com.youdeyiwu.mapper.forum.SectionMapper;
import com.youdeyiwu.mapper.user.UserMapper;
import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.dto.forum.ApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.NotApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.ReceivePostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.RefundPostReviewQueueDto;
import com.youdeyiwu.model.dto.point.PointAutoRuleEventDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostReviewQueueEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.other.UserContext;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.PostReviewQueueEntityVo;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.PostReviewQueueRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.PostReviewQueueService;
import java.time.LocalDate;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * post review queue.
 *
 * @author dafengzhen
 */
@Log4j2
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class PostReviewQueueServiceImpl implements PostReviewQueueService {

  private final PostReviewQueueRepository postReviewQueueRepository;

  private final PostRepository postRepository;

  private final UserRepository userRepository;

  private final SecurityService securityService;

  private final ApplicationEventPublisher publisher;

  private final PostMapper postMapper;

  private final PostReviewQueueMapper postReviewQueueMapper;

  private final UserMapper userMapper;

  private final SectionMapper sectionMapper;

  @Transactional
  @Override
  public void receive(ReceivePostReviewQueueDto dto) {
    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    LocalDate now = LocalDate.now();
    LocalDate latestReviewResultTime = dto.latestReviewResultTime();
    boolean isAfterOrEqual =
        latestReviewResultTime.isAfter(now) || latestReviewResultTime.isEqual(now);
    if (!isAfterOrEqual) {
      throw new CustomException("Is latestReviewResultTime after or equal to current date? ");
    }

    if (postEntity.getReviewState() != PostReviewStateEnum.PENDING_REVIEW) {
      throw new CustomException("You can only claim posts that are currently awaiting review");
    }

    boolean whetherToContinue = Boolean.TRUE.equals(userEntity.getRoot())
        || (Objects.nonNull(postEntity.getSection())
        && postEntity.getSection().getAdmins().contains(userEntity));
    if (!whetherToContinue) {
      throw new CustomException("Sorry, you are unable to process the audit request for this post");
    }

    PostReviewQueueEntity postReviewQueueEntity;
    if (postReviewQueueRepository.existsByPost(postEntity)) {
      PostReviewQueueEntity byPost = postReviewQueueRepository.findByPost(postEntity);
      if (Boolean.TRUE.equals(byPost.getReceived())) {
        throw new CustomException(
            "The audit request for this post has been claimed and cannot be claimed again");
      } else {
        byPost.setReceived(true);
        byPost.setLatestReviewResultTime(dto.latestReviewResultTime());
        byPost.setReceiver(userEntity);
        postReviewQueueEntity = byPost;
      }
    } else {
      PostReviewQueueEntity entity = new PostReviewQueueEntity();
      entity.setPost(postEntity);
      entity.setLatestReviewResultTime(dto.latestReviewResultTime());
      entity.setReceiver(userEntity);
      postEntity.setPostReviewQueue(entity);
      postReviewQueueEntity = postReviewQueueRepository.save(entity);
    }

    String currentDateTime = getCurrentDateTime();
    String estimatedTime = getCurrentDateTime(postReviewQueueEntity.getLatestReviewResultTime());
    sendReceivedMessageToReviewer(postReviewQueueEntity, currentDateTime, estimatedTime);
    sendReceivedMessageToUser(postReviewQueueEntity, currentDateTime, estimatedTime);
  }

  @Transactional
  @Override
  public void refund(Long id, RefundPostReviewQueueDto dto) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    if (Boolean.FALSE.equals(postReviewQueueEntity.getReceived())) {
      throw new CustomException("The audit request for this post has not been processed yet");
    }

    if (!Objects.equals(postReviewQueueEntity.getReceiver(), userEntity)) {
      throw new CustomException(
          "Sorry, you are unable to cancel the processing of the audit request for this post");
    }

    postReviewQueueEntity.setReceived(false);
    postReviewQueueEntity.setLatestReviewResultTime(null);
    String currentDateTime = getCurrentDateTime();
    sendRefundMessageToReviewer(postReviewQueueEntity, currentDateTime);
    sendRefundMessageToUser(postReviewQueueEntity, currentDateTime);
  }

  @Transactional
  @Override
  public void approved(Long id, ApprovedPostReviewQueueDto dto) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    if (Boolean.FALSE.equals(postReviewQueueEntity.getReceived())) {
      throw new CustomException("Sorry, the review for this post has not started yet");
    }

    if (!Objects.equals(postReviewQueueEntity.getReceiver(), userEntity)) {
      throw new CustomException(
          "Sorry, you are unable to process the review result of this post");
    }

    PostEntity postEntity = postReviewQueueEntity.getPost();
    postEntity.setReviewReason(dto.reason());
    postEntity.setReviewState(PostReviewStateEnum.APPROVED);
    postEntity.setPostReviewQueue(null);
    postReviewQueueRepository.delete(postReviewQueueEntity);
    String currentDateTime = getCurrentDateTime();
    sendApprovedMessageToUser(postReviewQueueEntity, currentDateTime, dto.reason());

    if (Objects.nonNull(postEntity.getUser())) {
      publisher.publishEvent(new PointAutoRuleApplicationEvent(
          new PointAutoRuleEventDto(
              AutoRuleNameEnum.POST_APPROVED,
              SignEnum.POSITIVE,
              postEntity.getId()
          )
      ));
    }
  }

  @Transactional
  @Override
  public void notApproved(Long id, NotApprovedPostReviewQueueDto dto) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    if (Boolean.FALSE.equals(postReviewQueueEntity.getReceived())) {
      throw new CustomException("Sorry, the review for this post has not started yet");
    }

    if (!Objects.equals(postReviewQueueEntity.getReceiver(), userEntity)) {
      throw new CustomException(
          "Sorry, you are unable to process the review result of this post");
    }

    PostEntity postEntity = postReviewQueueEntity.getPost();
    postEntity.setReviewReason(dto.reason());
    postEntity.setReviewState(PostReviewStateEnum.REJECTED);
    postEntity.setPostReviewQueue(null);
    postReviewQueueRepository.delete(postReviewQueueEntity);
    String currentDateTime = getCurrentDateTime();
    sendNotApprovedMessageToUser(postReviewQueueEntity, currentDateTime, dto.reason());

    if (Objects.nonNull(postEntity.getUser())) {
      publisher.publishEvent(new PointAutoRuleApplicationEvent(
          new PointAutoRuleEventDto(
              AutoRuleNameEnum.POST_NOT_APPROVED,
              SignEnum.NEGATIVE,
              postEntity.getId()
          )
      ));
    }
  }

  @Override
  public PostReviewQueueEntityVo query(Long id) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    PostReviewQueueEntityVo vo = postReviewQueueMapper.entityToVo(postReviewQueueEntity);
    vo.setPost(postMapper.entityToVo(postReviewQueueEntity.getPost()));
    vo.setReceiver(userMapper.entityToVo(postReviewQueueEntity.getReceiver()));
    return vo;
  }

  @Override
  public PageVo<PostEntityVo> queryAll(Pageable pageable) {
    UserContext userContext = securityService.getUserContext();
    Page<PostEntity> page = postRepository.findPostReviewQueues(
        new PaginationPositionDto(pageable),
        userContext.anonymous(),
        userContext.user(),
        userContext.root()
    );

    return new PageVo<>(
        page.map(postEntity -> {
              PostEntityVo vo = postMapper.entityToVo(postEntity);
              vo.setSection(sectionMapper.entityToVo(postEntity.getSection()));
              PostReviewQueueEntity postReviewQueueEntity = postEntity.getPostReviewQueue();
              PostReviewQueueEntityVo postReviewQueueEntityVo =
                  postReviewQueueMapper.entityToVo(postReviewQueueEntity);

              if (Objects.nonNull(postReviewQueueEntity)) {
                postReviewQueueEntityVo.setReceiver(
                    userMapper.entityToVo(postReviewQueueEntity.getReceiver()));
              }

              vo.setPostReviewQueue(postReviewQueueEntityVo);
              return vo;
            }
        )
    );
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