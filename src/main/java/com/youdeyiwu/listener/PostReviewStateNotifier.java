package com.youdeyiwu.listener;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PostReviewStateApplicationEvent;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.tool.I18nTool;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 * post review state listener.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class PostReviewStateNotifier
    implements ApplicationListener<PostReviewStateApplicationEvent> {

  private final ApplicationEventPublisher publisher;

  private final I18nTool i18nTool;

  @Override
  public void onApplicationEvent(PostReviewStateApplicationEvent event) {
    PostEntity entity = (PostEntity) event.getSource();
    String currentDateTime = getCurrentDateTime();
    String reviewReason = entity.getReviewReason();

    if (Objects.isNull(entity.getUser())) {
      return;
    }

    switch (entity.getReviewState()) {
      case APPROVED -> {
        sendPostReviewStateMessage(
            "post.reviewState.approved.user.message.name",
            "post.reviewState.approved.user.message.overview",
            Map.of(
                "time", currentDateTime,
                "reason", reviewReason
            ),
            entity,
            entity.getUser()
        );
      }
      case REJECTED -> {
        sendPostReviewStateMessage(
            "post.reviewState.rejected.user.message.name",
            "post.reviewState.rejected.user.message.overview",
            Map.of(
                "time", currentDateTime,
                "reason", reviewReason
            ),
            entity,
            entity.getUser()
        );
      }
      case PENDING_REVIEW -> {
        sendPostReviewStateMessage(
            "post.reviewState.pendingReview.user.message.name",
            "post.reviewState.pendingReview.user.message.overview",
            Map.of(
                "time", currentDateTime
            ),
            entity,
            entity.getUser()
        );
      }
      default -> throw new IllegalStateException("Unexpected value: " + entity.getReviewState());
    }
  }

  /**
   * send post review state message.
   *
   * @param nameCode     nameCode
   * @param overviewCode overviewCode
   * @param postEntity   postEntity
   * @param receiver     receiver
   */
  private void sendPostReviewStateMessage(
      String nameCode,
      String overviewCode,
      Map<String, Object> overviewArgs,
      PostEntity postEntity,
      UserEntity receiver
  ) {
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(i18nTool.getMessage(nameCode));
    messageEntity.setOverview(i18nTool.getMessage(overviewCode, overviewArgs));
    messageEntity.setLink(postEntity.getLink());
    messageEntity.setReceiver(receiver);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }
}
