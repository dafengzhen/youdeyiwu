package com.youdeyiwu.listener;

import static com.youdeyiwu.constant.PointConstant.POINT_REWARD_BY_SYSTEM;
import static com.youdeyiwu.tool.Tool.calculatePoints;
import static com.youdeyiwu.tool.Tool.getDifferenceSign;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PointAutoRuleApplicationEvent;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.point.PointAutoRuleEventDto;
import com.youdeyiwu.model.dto.point.PointAutoRuleProcessEventDto;
import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.point.PointAutoRuleEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.point.PointAutoRuleRepository;
import com.youdeyiwu.repository.point.PointHistoryRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.point.PointCoreService;
import com.youdeyiwu.service.point.PointService;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 * post auto rule listener.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class PointAutoRuleNotifier
    implements ApplicationListener<PointAutoRuleApplicationEvent> {

  private final PostRepository postRepository;

  private final PointAutoRuleRepository pointAutoRuleRepository;

  private final PointHistoryRepository pointHistoryRepository;

  private final UserRepository userRepository;

  private final ConfigRepository configRepository;

  private final SecurityService securityService;

  private final PointService pointService;

  private final PointCoreService pointCoreService;

  private final ApplicationEventPublisher publisher;

  @Override
  public void onApplicationEvent(PointAutoRuleApplicationEvent event) {
    ConfigEntity enable = configRepository.findByTypeAndName(
        ConfigTypeEnum.POINT,
        PointConfigConstant.ENABLE
    );

    if (Boolean.FALSE.equals(Boolean.valueOf(enable.getValue()))) {
      return;
    }

    PointAutoRuleEventDto dto = (PointAutoRuleEventDto) event.getSource();
    switch (dto.autoRuleName()) {
      case LIKED_YOUR_POST -> likedYourPost(dto);
      case LIKED_YOUR_COMMENT -> likedYourComment(dto);
      case LIKED_YOUR_REPLY -> likedYourReply(dto);
      case COMMENTED_ON_YOUR_POST -> commentedOnYourPost(dto);
      case REPLIED_TO_YOUR_POST -> repliedToYourPost(dto);
      case FOLLOWED_YOUR_POST -> followedYourPost(dto);
      case FAVORITED_YOUR_POST -> favoritedYourPost(dto);
      case DISLIKED_YOUR_POST -> dislikedYourPost(dto);
      case DISLIKED_YOUR_COMMENT -> dislikedYourComment(dto);
      case DISLIKED_YOUR_REPLY -> dislikedYourReply(dto);
      case POST_APPROVED -> postApproved(dto);
      case POST_NOT_APPROVED -> postNotApproved(dto);
      case POST_PENDING_REVIEW -> postPendingReview(dto);
      case VISITED_YOUR_POST -> visitedYourPost(dto);
      default -> throw new IllegalStateException("Unexpected value: " + dto.autoRuleName());
    }
  }

  /**
   * liked your post.
   *
   * @param dto dto
   */
  private void likedYourPost(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case POSITIVE:
              message = "Great! Through your like, you have received a new points reward";
              description = """
                  You liked the post [ %s ] and received a [ %s ] points reward automatically given by the system.
                  Thank you for your support!
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE:
              message = "Unfortunately, your points reward has been reduced";
              description = """
                  Due to the cancellation of the like on the post [ %s ],
                  the [ %s ] points reward automatically given by the system will be revoked.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! There has been no change in your points reward";
              description = """
                  Your points remain stable.
                  Try liking a post to receive an automatic points reward from the system
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * liked your comment.
   *
   * @param dto dto
   */
  private void likedYourComment(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case POSITIVE:
              message =
                  "Awesome! By liking a post's comment, you have received a new points reward";
              description = """
                  You have liked a comment on the post [ %s ] and received a [ %s ] points reward automatically given by the system.
                  Thank you for your support
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE:
              message = "Unfortunately, your points reward has decreased";
              description = """
                  Due to the cancellation of liking a comment on the post [ %s ],
                  the [ %s ] points reward automatically given by the system will be revoked.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! Your points reward has not changed";
              description = """
                  Your points remain stable.
                  Try giving a thumbs-up to comments on posts you like to receive automatic points rewards from the system
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * liked your reply.
   *
   * @param dto dto
   */
  private void likedYourReply(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case POSITIVE:
              message = "Great! By liking a reply to a post, you have received a new points reward";
              description = """
                  You have liked a reply to the post [ %s ] and received a [ %s ] points reward automatically given by the system.
                  Thank you for your support
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE:
              message = "Unfortunately, your points reward has decreased";
              description = """
                  Due to the cancellation of liking a reply to the post [ %s ],
                  the [ %s ] points reward automatically given by the system will be revoked.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! Your points reward has not changed";
              description = """
                  Your points remain stable.
                  Try giving a thumbs-up to replies on posts you like to receive automatic points rewards from the system
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * liked your comment.
   *
   * @param dto dto
   */
  private void commentedOnYourPost(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case POSITIVE:
              message = "Great! You have received a new points reward through your comments";
              description = """
                  You commented on the post [ %s ] and received a [ %s ] points reward automatically given by the system.
                  Thank you for your support!
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE:
              message = "Unfortunately, your points reward has been reduced";
              description = """
                  Due to the deletion of the comment on the post [ %s ],
                  the [ %s ] points reward automatically given by the system will be revoked.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! There has been no change in your points reward";
              description = """
                  Your points remain stable.
                  Try creating a comment for a post to receive an automatic points reward from the system
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * liked your reply.
   *
   * @param dto dto
   */
  private void repliedToYourPost(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case POSITIVE:
              message = "Great! Through your reply, you have received a new points reward";
              description = """
                  You replied to the post [ %s ] and received a [ %s ] points reward automatically given by the system.
                  Thank you for your support!
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE:
              message = "Unfortunately, your points reward has been reduced";
              description = """
                  Due to the deletion of the reply on the post [ %s ],
                  the [ %s ] points reward automatically given by the system will be revoked.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! There has been no change in your points reward";
              description = """
                  Your points remain stable.
                  Try creating a reply for a post to receive an automatic points reward from the system
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * followed your post.
   *
   * @param dto dto
   */
  private void followedYourPost(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case POSITIVE:
              message = "Great! By following the post, you have received a new points reward";
              description = """
                  You have followed the post [ %s ] and received a [ %s ] points reward automatically given by the system.
                  Thank you for your support
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE:
              message = "Unfortunately, your points reward has decreased";
              description = """
                  Due to the cancellation of following the post [ %s ],
                  the [ %s ] points reward automatically given by the system will be revoked.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! Your points reward has not changed";
              description = """
                  Your points remain stable.
                  Try following posts you like to receive automatic points rewards from the system
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * favorited your post.
   *
   * @param dto dto
   */
  private void favoritedYourPost(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case POSITIVE:
              message = "Great! You have received new points rewards by favoriting posts you like";
              description = """
                  You have favorited the post [ %s ] and received [ %s ] points as an automatic reward from the system.
                  Thank you for your support.
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE:
              message = "Unfortunately, your points reward has decreased";
              description = """
                  Due to the cancellation of favoriting the post [ %s ],
                  the system will reclaim the automatic reward of [ %s ] points.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! Your points reward remains unchanged";
              description = """
                  Your points are in a stable state.
                  Try favoriting posts you like to receive automatic points rewards from the system
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * disliked your post.
   *
   * @param dto dto
   */
  private void dislikedYourPost(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case NEGATIVE:
              message = "Received! You have disliked a post, and your points income has decreased";
              description = """
                  You have disliked the post [ %s ],
                  and the system will automatically deduct [ %s ] points from your account as a cost.
                  Thank you for your support and feedback
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case POSITIVE:
              message =
                  "Great! You have removed the dislike from a post, and your points income has increased";
              description = """
                  You have removed the dislike from the post [ %s ],
                  and the system will automatically reward you with [ %s ] points.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! Your points reward has not changed";
              description = """
                  Your points remain stable.
                  Try disliking posts you don't like as a way to provide feedback to the author
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * disliked your comment.
   *
   * @param dto dto
   */
  private void dislikedYourComment(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case NEGATIVE:
              message =
                  "Received! You have disliked a comment on a post, and your points income has decreased";
              description = """
                  You have disliked a comment on the post [ %s ],
                  and the system will automatically deduct [ %s ] points from your account as a cost.
                  Thank you for your support and feedback
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case POSITIVE:
              message =
                  "Great! You have removed the dislike from a comment on a post, and your points income has increased";
              description = """
                  You have removed the dislike from a comment on the post [ %s ],
                  and the system will automatically reward you with [ %s ] points.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! Your points reward has not changed";
              description = """
                  Your points remain stable.
                  Try disliking comments on posts you don't like as a way to express your feedback
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * disliked your reply.
   *
   * @param dto dto
   */
  private void dislikedYourReply(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case NEGATIVE:
              message =
                  "Received! You have disliked a reply to a post, and your points income has decreased";
              description = """
                  You have disliked a reply to the post [ %s ],
                  and the system will automatically deduct [ %s ] points from your account as a cost.
                  Thank you for your support and feedback
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case POSITIVE:
              message =
                  "Great! You have removed the dislike from a reply to a post, and your points income has increased";
              description = """
                  You have removed the dislike from a reply to the post [ %s ],
                  and the system will automatically reward you with [ %s ] points.
                  We look forward to interacting with you next time
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Stable! Your points reward has not changed";
              description = """
                  Your points remain stable.
                  Try disliking replies to posts you don't like as a way to express your feedback
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * post approved.
   *
   * @param dto dto
   */
  private void postApproved(PointAutoRuleEventDto dto) {
    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, postEntity.getUser().getId());
    if (Objects.isNull(result)) {
      return;
    }

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message = null;
          String description = null;
          String link = null;

          switch (sign) {
            case POSITIVE:
              message =
                  "Great! Your post [ %s ] has been approved, and you have received a new points reward";
              description = """
                  Your post [ %s ] has been approved,
                  and the system will automatically reward you with [ %s ] points.
                  Thank you for your support, and we look forward to your next exciting content
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE, ZERO:
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * post not approved.
   *
   * @param dto dto
   */
  private void postNotApproved(PointAutoRuleEventDto dto) {
    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, postEntity.getUser().getId());
    if (Objects.isNull(result)) {
      return;
    }

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message = null;
          String description = null;
          String link = null;

          switch (sign) {
            case NEGATIVE:
              message =
                  "Unfortunately, your post [ %s ] did not pass the review, and your points reward has been revoked";
              description = """
                  Your post [ %s ] did not pass the review,
                  and the system will automatically revoke [ %s ] points from your account as a cost.
                  We look forward to your revised post for resubmission
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case POSITIVE, ZERO:
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * post pending review.
   *
   * @param dto dto
   */
  private void postPendingReview(PointAutoRuleEventDto dto) {
    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, postEntity.getUser().getId());
    if (Objects.isNull(result)) {
      return;
    }

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message = null;
          String description = null;
          String link = null;

          switch (sign) {
            case NEGATIVE:
              message =
                  "Your post [ %s ] is currently awaiting review, and your points reward has been pre-allocated";
              description = """
                  Your post [ %s ] is currently awaiting review,
                  and the system will automatically pre-deduct [ %s ] points from your account as a cost.
                  We look forward to your update and manual resubmission for review
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case POSITIVE, ZERO:
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );

          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * visited your post.
   *
   * @param dto dto
   */
  private void visitedYourPost(PointAutoRuleEventDto dto) {
    PointAutoRuleProcessEventDto result = getProcessEventDto(dto, null);
    if (Objects.isNull(result)) {
      return;
    }

    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    getDifferenceSign(
        result.updatedPointEntity(),
        (sign, difference) -> {
          String message = null;
          String description = null;
          String link = null;

          switch (sign) {
            case POSITIVE:
              message =
                  "Great! Your post has been visited, and you have received a new point reward";
              description = """
                  Your post [ %s ] has been visited,
                  and you have been awarded [ %s ] points automatically by the system. Thank you for your support
                  """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE, ZERO:
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              result.updatedPointEntity(),
              difference,
              sign,
              dto.autoRuleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );

          sendMessage(message, description, link, result.userEntity());
        }
    );
  }

  /**
   * send message.
   *
   * @param name     name
   * @param overview overview
   * @param link     link
   * @param receiver receiver
   */
  private void sendMessage(
      String name,
      String overview,
      String link,
      UserEntity receiver
  ) {
    if (Objects.isNull(name) || Objects.isNull(overview)) {
      return;
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(name);
    messageEntity.setOverview(overview);
    messageEntity.setLink(link);
    messageEntity.setReceiver(receiver);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * get process event.
   *
   * @param dto    dto
   * @param userId userId
   * @return PointAutoRuleProcessEventDto
   */
  private PointAutoRuleProcessEventDto getProcessEventDto(PointAutoRuleEventDto dto, Long userId) {
    Optional<PointAutoRuleEntity> byAutoRuleName =
        pointAutoRuleRepository.findByAutoRuleName(dto.autoRuleName());
    if (byAutoRuleName.isEmpty()) {
      return null;
    }

    UserEntity userEntity = userRepository.findById(
            Objects.nonNull(userId)
                ? userId
                : securityService.getUserId()
        )
        .orElseThrow(UserNotFoundException::new);
    PointEntity updatedPointEntity = pointCoreService.update(
        pointService.findPointByUserEntity(userEntity),
        new UpdatePointDto(
            calculatePoints(
                byAutoRuleName.get().getRequiredPoints(),
                pointHistoryRepository
                    .findLatestPointsHistoryByUserIdAndAutoRuleName(userEntity.getId(),
                        dto.autoRuleName())
                    .map(pointHistoryEntity -> switch (pointHistoryEntity.getSign()) {
                      case POSITIVE -> SignEnum.NEGATIVE;
                      case NEGATIVE -> SignEnum.POSITIVE;
                      case ZERO -> SignEnum.ZERO;
                    })
                    .orElseGet(dto::sign)
            ),
            null,
            null
        )
    );
    return new PointAutoRuleProcessEventDto(userEntity, updatedPointEntity);
  }
}
