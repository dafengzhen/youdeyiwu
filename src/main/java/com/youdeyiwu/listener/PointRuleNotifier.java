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
import com.youdeyiwu.tool.I18nTool;
import java.util.HashSet;
import java.util.Map;
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

  private final I18nTool i18nTool;

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
      case LIKE_POST -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.likePost"));
      case LIKE_COMMENT -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.likeComment"));
      case LIKE_REPLY -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.likeReply"));
      case COMMENT_POST -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.commentPost"));
      case REPLY_POST -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.replyPost"));
      case FOLLOW_POST -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.followPost"));
      case FAVORITE_POST -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.favoritePost"));
      case DISLIKE_POST -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.dislikePost"));
      case DISLIKE_COMMENT -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.dislikeComment"));
      case DISLIKE_REPLY -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.dislikeReply"));
      case POST_APPROVED -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.postApproved"));
      case POST_NOT_APPROVED -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.postNotApproved"));
      case POST_PENDING_REVIEW -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.postPendingReview"));
      case VISIT_POST -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.visitPost"));
      case CREATE_POST -> handleActionsOnPosts(dto, pointRuleEntity, i18nTool.getMessage("point.createPost"));
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

    if (Boolean.TRUE.equals(dto.onlyInitiator())) {
      return;
    }

    Set<Long> userIds = new HashSet<>();

    if (!CollectionUtils.isEmpty(dto.receiverUserIds())) {
      userIds.addAll(dto.receiverUserIds());
    }

    if (Objects.nonNull(dto.postId())) {
      PostEntity postEntity = postRepository.findById(dto.postId())
          .orElseThrow(PostNotFoundException::new);

      if (Objects.equals(postEntity.getUser(), initiatorUser)) {
        return;
      }

      if (Objects.nonNull(postEntity.getUser())) {
        userIds.add(postEntity.getUser().getId());
      }

      userIds.forEach(userId -> updatePointsAndSendMessage(
          getUserEntity(userId),
          postEntity.getNameAndId(),
          postEntity.getLink(),
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
                      case ZERO -> {
                        if (Objects.isNull(dto.sign()) || dto.sign() == SignEnum.POSITIVE) {
                          yield SignEnum.ZERO;
                        } else {
                          yield SignEnum.NEGATIVE;
                        }
                      }
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
              Map.of(
                  "increased", flag == SignEnum.POSITIVE ? difference : 0,
                  "decreased", flag == SignEnum.NEGATIVE ? difference : 0,
                  "remaining", pointEntity.getPoints(),
                  "source", Objects.isNull(from) ? i18nTool.getMessage("point.systemService") : from
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
   * @param overviewArgs overviewArgs
   * @param link         link
   * @param receiver     receiver
   */
  private void sendMessage(
      Map<String, Object> overviewArgs,
      String link,
      UserEntity receiver
  ) {
    if (Objects.isNull(overviewArgs)) {
      return;
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(i18nTool.getMessage("point.pointRule.message.name"));
    messageEntity.setOverview(i18nTool.getMessage("point.pointRule.message.overview", overviewArgs));
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
   * get user entity.
   *
   * @param userId userId
   * @return UserEntity
   */
  private UserEntity getUserEntity(Long userId) {
    return userRepository.findById(userId).orElseThrow(UserNotFoundException::new);
  }
}
