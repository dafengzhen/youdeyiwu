package com.youdeyiwu.listener;

import static com.youdeyiwu.constant.PointConstant.POINT_REWARD_BY_SYSTEM;
import static com.youdeyiwu.tool.Tool.calculatePoints;
import static com.youdeyiwu.tool.Tool.getDifferenceSign;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PointRuleApplicationEvent;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.point.PointRuleEventDto;
import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointRuleEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
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
    Optional<PointRuleEntity> byRuleName = pointRuleRepository.findByRuleName(dto.ruleName());
    if (byRuleName.isEmpty() || Boolean.FALSE.equals(byRuleName.get().getEnable())) {
      return;
    }

    handleActions(dto, byRuleName.get());
  }

  /**
   * handle actions.
   *
   * @param dto             dto
   * @param pointRuleEntity pointRuleEntity
   */
  private void handleActions(PointRuleEventDto dto, PointRuleEntity pointRuleEntity) {
    Set<Long> userIds = new HashSet<>();
    if (securityService.isAuthenticated()) {
      userIds.add(securityService.getUserId());
    }

    if (!CollectionUtils.isEmpty(dto.receivedUserIds())) {
      userIds.addAll(dto.receivedUserIds());
    }

    userIds.forEach(userId -> updatePointsAndSendMessage(
        userRepository.findById(userId).orElseThrow(UserNotFoundException::new),
        dto,
        pointRuleEntity
    ));
  }

  /**
   * Update points and send messages for a user.
   *
   * @param user            user
   * @param dto             dto
   * @param pointRuleEntity pointRuleEntity
   */
  private void updatePointsAndSendMessage(
      UserEntity user,
      PointRuleEventDto dto,
      PointRuleEntity pointRuleEntity
  ) {
    SignEnum sign;
    if (Boolean.TRUE.equals(dto.checkHistoryPoints())) {
      sign = pointHistoryRepository.findLatestPointsHistoryByUserIdAndRuleName(
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
          .orElseGet(dto::sign);
    } else {
      sign = dto.sign();
    }

    Integer rewardPoints;
    if (dto.receivedUserIds().contains(user.getId())) {
      rewardPoints = pointRuleEntity.getReceiverRewardPoints();
    } else {
      rewardPoints = pointRuleEntity.getInitiatorRewardPoints();
    }

    PointEntity pointEntity = pointCoreService.update(
        pointService.findPointByUserEntity(user),
        new UpdatePointDto(calculatePoints(rewardPoints, sign), null, null)
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
                  "source", Objects.isNull(dto.from()) ? i18nTool.getMessage("point.systemService") : dto.from()
              ),
              dto.link(),
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
}
