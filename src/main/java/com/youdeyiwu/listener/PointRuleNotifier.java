package com.youdeyiwu.listener;

import static com.youdeyiwu.constant.PointConstant.POINT_REWARD_BY_SYSTEM;
import static com.youdeyiwu.tool.Tool.calculatePoints;
import static com.youdeyiwu.tool.Tool.getDifferenceSign;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PointRuleApplicationEvent;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.point.PointRuleEventDto;
import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointRuleEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.point.PointHistoryRepository;
import com.youdeyiwu.repository.point.PointRuleRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.point.PointCoreService;
import com.youdeyiwu.service.point.PointService;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

/**
 * post rule listener.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class PointRuleNotifier
    implements ApplicationListener<PointRuleApplicationEvent> {

  private final PostRepository postRepository;

  private final PointRuleRepository pointRuleRepository;

  private final PointHistoryRepository pointHistoryRepository;

  private final UserRepository userRepository;

  private final ConfigRepository configRepository;

  private final SecurityService securityService;

  private final PointService pointService;

  private final PointCoreService pointCoreService;

  private final ApplicationEventPublisher publisher;

  @Override
  public void onApplicationEvent(PointRuleApplicationEvent event) {
    ConfigEntity enable = configRepository.findByTypeAndName(
        ConfigTypeEnum.POINT,
        PointConfigConstant.ENABLE
    );

    if (Boolean.FALSE.equals(Boolean.valueOf(enable.getValue()))) {
      return;
    }

    PointRuleEventDto dto = (PointRuleEventDto) event.getSource();
    Optional<PointRuleEntity> byRuleName =
        pointRuleRepository.findByRuleName(dto.ruleName());
    if (byRuleName.isEmpty()) {
      return;
    }

    PointRuleEntity pointRuleEntity = byRuleName.get();
    switch (dto.ruleName()) {
      case LIKE_POST -> handleActionsOnPosts(dto, pointRuleEntity, "Like Post");
      case LIKE_COMMENT -> handleActionsOnPosts(dto, pointRuleEntity, "Like Comment");
      case LIKE_REPLY -> handleActionsOnPosts(dto, pointRuleEntity, "Like Reply");
      case COMMENT_POST -> handleActionsOnPosts(dto, pointRuleEntity, "Create Comment");
      case REPLY_POST -> handleActionsOnPosts(dto, pointRuleEntity, "Create Reply");
      case FOLLOW_POST -> handleActionsOnPosts(dto, pointRuleEntity, "Follow Post");
      case FAVORITE_POST -> handleActionsOnPosts(dto, pointRuleEntity, "Favorite Post");
      case DISLIKE_POST -> handleActionsOnPosts(dto, pointRuleEntity, "Dislike Post");
      case DISLIKE_COMMENT -> handleActionsOnPosts(dto, pointRuleEntity, "Dislike Comment");
      case DISLIKE_REPLY -> handleActionsOnPosts(dto, pointRuleEntity, "Dislike Reply");
      case POST_APPROVED -> handleActionsOnPosts(dto, pointRuleEntity, "Post Approved");
      case POST_NOT_APPROVED -> handleActionsOnPosts(dto, pointRuleEntity, "Post Not Approved");
      case POST_PENDING_REVIEW -> handleActionsOnPosts(dto, pointRuleEntity, "Post Pending Review");
      case VISIT_POST -> handleActionsOnPosts(dto, pointRuleEntity, "Visit Post");
      case CREATE_POST -> handleActionsOnPosts(dto, pointRuleEntity, "Create Post");
      default -> throw new IllegalStateException("Unexpected value: " + dto.ruleName());
    }
  }

  /**
   * handle actions on posts.
   *
   * @param dto             dto
   * @param pointRuleEntity pointRuleEntity
   * @param from            from
   */
  private void handleActionsOnPosts(
      PointRuleEventDto dto,
      PointRuleEntity pointRuleEntity,
      String from
  ) {
    UserEntity initiatorUser = getInitiatorUser();

    if (Objects.nonNull(initiatorUser)) {
      updatePointsAndSendMessage(initiatorUser, dto.from(), dto.link(), true, dto, pointRuleEntity);
    }

    if (Boolean.FALSE.equals(dto.onlyInitiator())) {
      Set<Long> userIds = new HashSet<>();

      if (!CollectionUtils.isEmpty(dto.receiverUserIds())) {
        userIds.addAll(dto.receiverUserIds());
      }

      if (Objects.nonNull(dto.postId())) {
        PostEntity postEntity = getPostEntity(dto.postId(), initiatorUser);
        if (Objects.nonNull(postEntity.getUser())) {
          userIds.add(postEntity.getUser().getId());
        }

        userIds.forEach(userId -> updatePointsAndSendMessage(
            getUserEntity(userId),
            """
                %s [ %s ]
                """
                .formatted(postEntity.getName(), from),
            "/posts/" + postEntity.getId(),
            false,
            dto,
            pointRuleEntity
        ));
      } else {
        userIds.forEach(userId -> updatePointsAndSendMessage(
            getUserEntity(userId),
            dto.from(),
            dto.link(),
            false,
            dto,
            pointRuleEntity
        ));
      }
    }
  }

  /**
   * Update points and send messages for a user.
   *
   * @param user            user
   * @param from            from
   * @param link            link
   * @param isInitiator     isInitiator
   * @param dto             dto
   * @param pointRuleEntity pointRuleEntity
   */
  private void updatePointsAndSendMessage(
      UserEntity user,
      String from,
      String link,
      Boolean isInitiator,
      PointRuleEventDto dto,
      PointRuleEntity pointRuleEntity
  ) {
    if (Objects.isNull(user)) {
      return;
    }

    PointEntity pointEntity = pointCoreService.update(
        pointService.findPointByUserEntity(user),
        new UpdatePointDto(
            calculatePoints(
                Boolean.TRUE.equals(isInitiator)
                    ? pointRuleEntity.getInitiatorRewardPoints()
                    : pointRuleEntity.getReceiverRewardPoints(),
                pointHistoryRepository.findLatestPointsHistoryByUserIdAndRuleName(
                        user.getId(),
                        dto.ruleName()
                    )
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
        pointEntity,
        (flag, difference) -> {
          pointCoreService.create(
              pointEntity,
              difference,
              flag,
              dto.ruleName(),
              null,
              POINT_REWARD_BY_SYSTEM
          );
          sendMessage(
              """
                  Your points have changed, originating from [ %s ].
                  Points have increased by [ %s ], decreased by [ %s ], and the remaining points are [ %s ]
                  """
                  .formatted(
                      Objects.isNull(from) ? "System service" : from,
                      flag == SignEnum.POSITIVE ? difference : 0,
                      flag == SignEnum.NEGATIVE ? difference : 0,
                      pointEntity.getPoints()
                  ),
              link,
              user
          );
        }
    );
  }

  /**
   * send message.
   *
   * @param overview overview
   * @param link     link
   * @param receiver receiver
   */
  private void sendMessage(
      String overview,
      String link,
      UserEntity receiver
  ) {
    if (Objects.isNull(overview)) {
      return;
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("Your points have changed");
    messageEntity.setOverview(overview);
    messageEntity.setLink(link);
    messageEntity.setReceiver(receiver);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * get initiator user.
   *
   * @return UserEntity
   */
  private UserEntity getInitiatorUser() {
    return securityService.isAuthenticated()
        ? userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new)
        : null;
  }

  /**
   * get post entity.
   *
   * @param postId        postId
   * @param initiatorUser initiatorUser
   * @return PostEntity
   */
  private PostEntity getPostEntity(Long postId, UserEntity initiatorUser) {
    return postRepository.findById(postId)
        .filter(postEntity -> !Objects.equals(initiatorUser, postEntity.getUser()))
        .orElseThrow(PostNotFoundException::new);
  }

  /**
   * get user entity.
   *
   * @param userId userId
   * @return UserEntity
   */
  private UserEntity getUserEntity(Long userId) {
    return userRepository.findById(userId).orElseThrow(UserNotFoundException::new);
  }
}
