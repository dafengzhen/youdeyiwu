package com.youdeyiwu.listener;

import static com.youdeyiwu.tool.Tool.getDifferenceSign;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PointAutoRuleApplicationEvent;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.point.PointAutoRuleEventDto;
import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.point.PointAutoRuleEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.point.PointAutoRuleRepository;
import com.youdeyiwu.repository.point.PointHistoryRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.point.PointCoreService;
import com.youdeyiwu.service.point.PointService;
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
      case LIKED_YOUR_COMMENT -> {
      }
      case LIKED_YOUR_REPLY -> {
      }
      case COMMENTED_ON_YOUR_POST -> {
      }
      case REPLIED_TO_YOUR_POST -> {
      }
      case FOLLOWED_YOUR_POST -> {
      }
      case BOOKMARKED_YOUR_POST -> {
      }
      case APPRECIATED_YOUR_POST -> {
      }
      case DISLIKED_YOUR_POST -> {
      }
      case DISLIKED_YOUR_COMMENT -> {
      }
      case DISLIKED_YOUR_REPLY -> {
      }
      case POST_NOT_APPROVED -> {
      }
      case POST_UNDER_REVIEW -> {
      }
      case VISITED_YOUR_POST -> {
      }
      default -> throw new IllegalStateException("Unexpected value: " + dto.autoRuleName());
    }
  }

  /**
   * liked your post.
   *
   * @param dto dto
   */
  private void likedYourPost(PointAutoRuleEventDto dto) {
    Optional<PointAutoRuleEntity> byAutoRuleName = pointAutoRuleRepository.findByAutoRuleName(dto.autoRuleName());
    if (byAutoRuleName.isEmpty()) {
      return;
    }

    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);

    PointEntity pointEntity = pointService.findPointByUserEntity(userEntity);
    PointAutoRuleEntity pointAutoRuleEntity = byAutoRuleName.get();
    Optional<PointHistoryEntity> pointHistoryEntityOptional = pointHistoryRepository
        .findLatestPointsHistoryByUserIdAndAutoRuleName(userEntity.getId(), dto.autoRuleName());
    PointEntity updatedPointEntity = pointCoreService.update(
        pointEntity,
        new UpdatePointDto(
            calculatePoints(
                pointAutoRuleEntity.getRequiredPoints(),
                pointHistoryEntityOptional
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

    getDifferenceSign(
        updatedPointEntity,
        (sign, difference) -> {
          String message;
          String description;
          String link;

          switch (sign) {
            case POSITIVE:
              message = "Awesome! You have earned new like points";
              description = """
                    Congratulations! Due to your liking of the post [%s],
                    you have been awarded %s points as a gift from the system. Please continue to support us!
                    """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case NEGATIVE:
              message = "Unfortunately! Your like points have been reduced";
              description = """
                    Unfortunately, due to your unliking of the post [%s],
                    the system will reclaim %s points that were previously awarded.
                    We will continue to strive and look forward to earning your support again!
                    """
                  .formatted(
                      postEntity.getName(),
                      difference
                  );
              link = "/posts/" + postEntity.getId();
              break;
            case ZERO:
              message = "Steady! Your like points neither increased nor decreased";
              description = """
                    Nothing eventful, your point count remains unchanged.
                    Trying to like posts you support is also a good choice to increase your points. Give it a try and see your points rise!
                  """;
              link = null;
              break;
            default:
              throw new IllegalStateException("Unexpected value: " + sign);
          }

          pointCoreService.create(
              updatedPointEntity,
              difference,
              sign,
              dto.autoRuleName(),
              null,
              null
          );
          sendMessage(message, description, link, userEntity);
        }
    );
  }

  /**
   * calculate points.
   *
   * @param requiredPoints requiredPoints
   * @param sign           sign
   * @return Integer
   */
  private Integer calculatePoints(Integer requiredPoints, SignEnum sign) {
    return switch (sign) {
      case POSITIVE -> requiredPoints;
      case NEGATIVE -> -requiredPoints;
      case ZERO -> 0;
    };
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
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(name);
    messageEntity.setOverview(overview);
    messageEntity.setLink(link);
    messageEntity.setReceiver(receiver);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }
}
