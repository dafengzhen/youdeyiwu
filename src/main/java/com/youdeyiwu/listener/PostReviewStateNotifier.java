package com.youdeyiwu.listener;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PostReviewStateApplicationEvent;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

/**
 * post review state listener.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class PostReviewStateNotifier
    implements ApplicationListener<PostReviewStateApplicationEvent> {

  private final PostRepository postRepository;

  private final UserRepository userRepository;

  private final ApplicationEventPublisher publisher;

  private final SecurityService securityService;

  @Override
  public void onApplicationEvent(PostReviewStateApplicationEvent event) {
    PostEntity entity = (PostEntity) event.getSource();
    PostEntity postEntity = postRepository.findById(entity.getId())
        .orElseThrow(PostNotFoundException::new);

    if (
        Objects.equals(entity.getReviewState(), postEntity.getReviewState())
            || Objects.isNull(entity.getUser())
    ) {
      return;
    }

    Optional<UserEntity> sender = securityService.isAnonymous()
        ? Optional.empty()
        : userRepository.findById(securityService.getUserId());

    String senderAlias = sender.map(securityService::getAliasAndId).orElse(null);
    String currentDateTime = getCurrentDateTime();
    String reviewReason = StringUtils.hasText(entity.getReviewReason())
        ? entity.getReviewReason()
        : "Review reason not filled in.";

    switch (entity.getReviewState()) {
      case APPROVED -> {
        String message = Objects.isNull(senderAlias)
            ?
            "The system has approved your post [%s] at %s. Your post is now accessible."
                .formatted(entity.getName(), currentDateTime)
            : "User %s has approved your post [%s] at %s. Your post is now accessible."
            .formatted(senderAlias, entity.getName(), currentDateTime);

        sendPostReviewStateMessage(
            "Congratulations, your post has been approved",
            message,
            entity.getId(),
            entity.getUser());
      }
      case REJECTED -> {
        String message = (Objects.isNull(senderAlias))
            ?
            "The system has rejected your post [%s] at %s. Your post is temporarily inaccessible. The reason for the rejection is as follows: %s"
                .formatted(entity.getName(), currentDateTime, reviewReason)
            :
            "User %s has rejected your post [%s] at %s. Your post is temporarily inaccessible. The reason for the rejection is as follows: %s"
                .formatted(senderAlias, entity.getName(), currentDateTime, reviewReason);

        sendPostReviewStateMessage("Sorry, your post did not pass the review", message,
            entity.getId(),
            entity.getUser());
      }
      case PENDING_REVIEW -> {
        String message =
            """
                Success is just around the corner.
                Your post [%s] is currently awaiting review. Once approved, it will be accessible to the public.
                Please note that during the review process, you won't be able to edit the post.
                Please wait for the review to be completed.
                """
                .formatted(entity.getName());

        sendPostReviewStateMessage(
            "Next step in progress, your post is awaiting review",
            message,
            entity.getId(),
            entity.getUser()
        );
        notifySectionAdmins(entity);
      }
      default -> throw new IllegalStateException("Unexpected value: " + entity.getReviewState());
    }
  }

  /**
   * notify section admins.
   *
   * @param entity entity
   */
  private void notifySectionAdmins(PostEntity entity) {
    if (Objects.nonNull(entity.getSection()) && !entity.getSection().getAdmins().isEmpty()) {
      return;
    }

    entity.getSection().getAdmins()
        .forEach(userEntity -> sendPostReviewStateMessage(
            "There is a post awaiting your review",
            """
                There is a post [%s] in the forum section you manage [%s] that is currently awaiting review.
                Please go to the backend's review queue to check and process it.
                """
                .formatted(entity.getName(), entity.getSection().getName()),
            entity.getId(),
            entity.getUser()
        ));
  }

  /**
   * send post review state message.
   *
   * @param name     name
   * @param overview overview
   * @param postId   postId
   * @param receiver receiver
   */
  private void sendPostReviewStateMessage(
      String name,
      String overview,
      Long postId,
      UserEntity receiver
  ) {
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(name);
    messageEntity.setOverview(overview);
    messageEntity.setLink("/posts/" + postId);
    messageEntity.setReceiver(receiver);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }
}
