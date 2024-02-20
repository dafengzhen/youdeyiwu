package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.PostReviewQueueNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.forum.ApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.NotApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.ReceivePostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.ReturnPostReviewQueueDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostReviewQueueEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.PostReviewQueueRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.tool.I18nTool;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

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

  private final I18nTool i18nTool;

  /**
   * receive.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostReviewQueueServiceImpl.receive(..)) && args(dto)", argNames = "dto")
  public void receivePointcut(ReceivePostReviewQueueDto dto) {
    // Pointcut
  }

  /**
   * return.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.PostReviewQueueServiceImpl.refund(..)) && args(id,dto)", argNames = "id,dto")
  public void returnPointcut(Long id, ReturnPostReviewQueueDto dto) {
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
    handlePostReviewAction("receive", dto.postId(), null, null, getCurrentDateTime(dto.latestReviewResultTime()));
  }

  /**
   * after advice.
   */
  @After(value = "returnPointcut(id,dto)", argNames = "id,dto")
  public void returnAfterAdvice(Long id, ReturnPostReviewQueueDto dto) {
    handlePostReviewAction("return", null, id, dto.reason(), null);
  }

  /**
   * after advice.
   */
  @After(value = "approvedPointcut(id,dto)", argNames = "id,dto")
  public void approvedAfterAdvice(Long id, ApprovedPostReviewQueueDto dto) {
    handlePostReviewAction("approved", null, id, dto.reason(), null);
  }

  /**
   * after advice.
   */
  @After(value = "notApprovedPointcut(id,dto)", argNames = "id,dto")
  public void notApprovedAfterAdvice(Long id, NotApprovedPostReviewQueueDto dto) {
    handlePostReviewAction("notApproved", null, id, dto.reason(), null);
  }

  /**
   * handle post review action.
   *
   * @param action        action
   * @param postId        postId
   * @param queueId       queueId
   * @param reason        reason
   * @param estimatedTime estimatedTime
   */
  private void handlePostReviewAction(String action, Long postId, Long queueId, String reason, String estimatedTime) {
    PostEntity postEntity = Objects.nonNull(postId)
        ? postRepository.findById(postId).orElseThrow(PostNotFoundException::new)
        : null;
    PostReviewQueueEntity postReviewQueueEntity =
        Objects.nonNull(queueId)
            ? postReviewQueueRepository.findById(queueId).orElseThrow(PostReviewQueueNotFoundException::new)
            : null;
    UserEntity userEntity = userRepository.findById(securityService.getUserId()).orElseThrow(UserNotFoundException::new);
    String currentDateTime = getCurrentDateTime();

    switch (action) {
      case "receive":
        if (Objects.nonNull(postEntity)) {
          sendReceiveMessageToReviewer(postEntity, userEntity, currentDateTime, estimatedTime);
          sendReceiveMessageToUser(postEntity, userEntity, currentDateTime, estimatedTime);
        }
        break;
      case "return":
        if (Objects.nonNull(postReviewQueueEntity)) {
          sendReturnMessageToReviewer(postReviewQueueEntity.getPost(), userEntity, currentDateTime, reason);
          sendReturnMessageToUser(postReviewQueueEntity.getPost(), userEntity, currentDateTime, reason);
        }
        break;
      case "approved":
        if (Objects.nonNull(postReviewQueueEntity)) {
          sendApprovedMessageToReviewer(postReviewQueueEntity.getPost(), userEntity, currentDateTime, reason);
          sendApprovedMessageToUser(postReviewQueueEntity.getPost(), userEntity, currentDateTime, reason);
        }
        break;
      case "notApproved":
        if (Objects.nonNull(postReviewQueueEntity)) {
          sendNotApprovedMessageToReviewer(postReviewQueueEntity.getPost(), userEntity, currentDateTime, reason);
          sendNotApprovedMessageToUser(postReviewQueueEntity.getPost(), userEntity, currentDateTime, reason);
        }
        break;
      default:
        throw new IllegalStateException("Unexpected value: " + action);
    }
  }

  /**
   * send receive message to reviewer.
   *
   * @param postEntity      postEntity
   * @param userEntity      userEntity
   * @param currentDateTime currentDateTime
   * @param estimatedTime   estimatedTime
   */
  private void sendReceiveMessageToReviewer(
      PostEntity postEntity,
      UserEntity userEntity,
      String currentDateTime,
      String estimatedTime
  ) {
    MessageEntity messageEntity = createMessageEntity(
        "postReviewQueue.receive.message.name",
        postEntity,
        userEntity,
        currentDateTime,
        estimatedTime,
        null,
        null
    );
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send receive message to user.
   *
   * @param postEntity      postEntity
   * @param userEntity      userEntity
   * @param currentDateTime currentDateTime
   * @param estimatedTime   estimatedTime
   */
  private void sendReceiveMessageToUser(
      PostEntity postEntity,
      UserEntity userEntity,
      String currentDateTime,
      String estimatedTime
  ) {
    if (Objects.isNull(postEntity.getUser())) {
      return;
    }

    MessageEntity messageEntity = createMessageEntity(
        "postReviewQueue.receive.user.message.name",
        postEntity,
        userEntity,
        currentDateTime,
        estimatedTime,
        null,
        securityService.getAliasAndId(userEntity)
    );
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send return message to reviewer.
   *
   * @param postEntity      postEntity
   * @param userEntity      userEntity
   * @param currentDateTime currentDateTime
   * @param reason          reason
   */
  private void sendReturnMessageToReviewer(
      PostEntity postEntity,
      UserEntity userEntity,
      String currentDateTime,
      String reason
  ) {
    MessageEntity messageEntity = createMessageEntity(
        "postReviewQueue.return.message.name",
        postEntity,
        userEntity,
        currentDateTime,
        null,
        reason,
        null
    );
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send return message to user.
   *
   * @param postEntity      postEntity
   * @param userEntity      userEntity
   * @param currentDateTime currentDateTime
   * @param reason          reason
   */
  private void sendReturnMessageToUser(
      PostEntity postEntity,
      UserEntity userEntity,
      String currentDateTime,
      String reason
  ) {
    if (Objects.isNull(postEntity.getUser())) {
      return;
    }

    MessageEntity messageEntity = createMessageEntity(
        "postReviewQueue.return.user.message.name",
        postEntity,
        userEntity,
        currentDateTime,
        null,
        reason,
        securityService.getAliasAndId(userEntity)
    );
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send approved message to reviewer.
   *
   * @param postEntity      postEntity
   * @param userEntity      userEntity
   * @param currentDateTime currentDateTime
   * @param reason          reason
   */
  private void sendApprovedMessageToReviewer(
      PostEntity postEntity,
      UserEntity userEntity,
      String currentDateTime,
      String reason
  ) {
    MessageEntity messageEntity = createMessageEntity(
        "postReviewQueue.approved.message.name",
        postEntity,
        userEntity,
        currentDateTime,
        null,
        reason,
        null
    );
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send approved message to user.
   *
   * @param postEntity      postEntity
   * @param userEntity      userEntity
   * @param currentDateTime currentDateTime
   * @param reason          reason
   */
  private void sendApprovedMessageToUser(
      PostEntity postEntity,
      UserEntity userEntity,
      String currentDateTime,
      String reason
  ) {
    if (Objects.isNull(postEntity.getUser())) {
      return;
    }

    MessageEntity messageEntity = createMessageEntity(
        "postReviewQueue.approved.user.message.name",
        postEntity,
        userEntity,
        currentDateTime,
        null,
        reason,
        securityService.getAliasAndId(userEntity)
    );
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send not approved message to reviewer.
   *
   * @param postEntity      postEntity
   * @param userEntity      userEntity
   * @param currentDateTime currentDateTime
   * @param reason          reason
   */
  private void sendNotApprovedMessageToReviewer(
      PostEntity postEntity,
      UserEntity userEntity,
      String currentDateTime,
      String reason
  ) {
    MessageEntity messageEntity = createMessageEntity(
        "postReviewQueue.notApproved.message.name",
        postEntity,
        userEntity,
        currentDateTime,
        null,
        reason,
        null
    );
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * send not approved message to user.
   *
   * @param postEntity      postEntity
   * @param userEntity      userEntity
   * @param currentDateTime currentDateTime
   * @param reason          reason
   */
  private void sendNotApprovedMessageToUser(
      PostEntity postEntity,
      UserEntity userEntity,
      String currentDateTime,
      String reason
  ) {
    if (Objects.isNull(postEntity.getUser())) {
      return;
    }

    MessageEntity messageEntity = createMessageEntity(
        "postReviewQueue.notApproved.user.message.name",
        postEntity,
        userEntity,
        currentDateTime,
        null,
        reason,
        securityService.getAliasAndId(userEntity)
    );
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * create message entity.
   *
   * @param code            code
   * @param postEntity      postEntity
   * @param userEntity      userEntity
   * @param currentDateTime currentDateTime
   * @param estimatedTime   estimatedTime
   * @param reason          reason
   * @param admin           admin
   * @return MessageEntity
   */
  private MessageEntity createMessageEntity(
      String code,
      PostEntity postEntity,
      UserEntity userEntity,
      String currentDateTime,
      String estimatedTime,
      String reason,
      String admin
  ) {
    Map<String, Object> overviewArgs = new HashMap<>();
    overviewArgs.put("name", postEntity.getNameAndId());
    overviewArgs.put("time", currentDateTime);
    overviewArgs.put("completeTime", estimatedTime);
    overviewArgs.put("reason", reason);
    overviewArgs.put("admin", admin);

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(i18nTool.getMessage(code));
    messageEntity.setOverview(i18nTool.getMessage(code, overviewArgs));
    messageEntity.setLink(postEntity.getLink());
    messageEntity.setReceiver(userEntity);
    return messageEntity;
  }
}
