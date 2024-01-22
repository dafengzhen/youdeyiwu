package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.enums.forum.PostReviewStateEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.PostReviewQueueNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.PostMapper;
import com.youdeyiwu.mapper.forum.PostReviewQueueMapper;
import com.youdeyiwu.mapper.user.UserMapper;
import com.youdeyiwu.model.dto.forum.ReceivePostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.RefundPostReviewQueueDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostReviewQueueEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
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
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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

  @Transactional
  @Override
  public void receive(Long id, ReceivePostReviewQueueDto dto) {
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

    boolean whetherToContinue = false;
    if (Boolean.TRUE.equals(userEntity.getRoot())) {
      whetherToContinue = true;
    } else if (
        Objects.nonNull(postEntity.getSection())
            && postEntity.getSection().getAdmins().contains(userEntity)
    ) {
      whetherToContinue = true;
    }

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
      postReviewQueueEntity = postReviewQueueRepository.save(entity);
    }

    String currentDateTime = getCurrentDateTime();
    String estimatedTime = getCurrentDateTime(postReviewQueueEntity.getLatestReviewResultTime());
    messageToReviewer(postReviewQueueEntity, currentDateTime, estimatedTime);
    messageToUser(postReviewQueueEntity, currentDateTime, estimatedTime);
  }

  @Transactional
  @Override
  public void refund(Long id, RefundPostReviewQueueDto dto) {
    // TODO
  }

  @Override
  public PostReviewQueueEntityVo query(Long id) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    return postReviewQueueMapper.entityToVo(postReviewQueueEntity);
  }

  @Override
  public PageVo<PostReviewQueueEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(postReviewQueueRepository.findAll(pageable).map(postReviewQueueEntity -> {
      PostReviewQueueEntityVo vo =
          postReviewQueueMapper.entityToVo(postReviewQueueEntity);
      vo.setPost(postMapper.entityToVo(postReviewQueueEntity.getPost()));
      vo.setReceiver(userMapper.entityToVo(postReviewQueueEntity.getReceiver()));
      return vo;
    }));
  }

  @Transactional
  @Override
  public void delete(Long id) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    postReviewQueueRepository.delete(postReviewQueueEntity);
  }

  /**
   * message to reviewer.
   *
   * @param postReviewQueueEntity postReviewQueueEntity
   * @param currentDateTime       currentDateTime
   * @param estimatedTime         estimatedTime
   */
  private void messageToReviewer(
      PostReviewQueueEntity postReviewQueueEntity,
      String currentDateTime,
      String estimatedTime
  ) {
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("You have received a post review request");

    if (Objects.isNull(postReviewQueueEntity.getPost().getUser())) {
      messageEntity.setOverview(
          """
              At %s, you have claimed the review request for the post [%s],
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
              You have received a post review request from user %s for the post [%s] at %s.
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
   * message to user.
   *
   * @param postReviewQueueEntity postReviewQueueEntity
   * @param currentDateTime       currentDateTime
   * @param estimatedTime         estimatedTime
   */
  private void messageToUser(
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
            Your post [%s] will be reviewed by user %s starting at %s.
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
}