package com.youdeyiwu.listener;

import static com.youdeyiwu.constant.PointConstant.POINT_REWARD_BY_SYSTEM;
import static com.youdeyiwu.tool.Tool.getDifferenceSign;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PointPermissionRuleApplicationEvent;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.point.PointPermissionRuleEventDto;
import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointPermissionRuleEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.point.PointPermissionRuleRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.point.PointCoreService;
import com.youdeyiwu.service.point.PointService;
import com.youdeyiwu.tool.I18nTool;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 * post permission rule listener.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class PointPermissionRuleNotifier
    implements ApplicationListener<PointPermissionRuleApplicationEvent> {

  private final PointPermissionRuleRepository pointPermissionRuleRepository;

  private final ConfigRepository configRepository;

  private final SecurityService securityService;

  private final PointService pointService;

  private final PointCoreService pointCoreService;

  private final I18nTool i18nTool;

  private final ApplicationEventPublisher publisher;

  private final UserRepository userRepository;

  @Override
  public void onApplicationEvent(PointPermissionRuleApplicationEvent event) {
    ConfigEntity enable = configRepository.findByTypeAndName(
        ConfigTypeEnum.POINT,
        PointConfigConstant.ENABLE
    );

    if (Boolean.FALSE.equals(Boolean.valueOf(enable.getValue()))) {
      return;
    }

    PointPermissionRuleEventDto dto = (PointPermissionRuleEventDto) event.getSource();
    Optional<PointPermissionRuleEntity> byPermissionRuleName =
        pointPermissionRuleRepository.findByPermissionRuleName(dto.permissionRuleName());
    if (byPermissionRuleName.isEmpty()) {
      return;
    }

    PointPermissionRuleEntity pointPermissionRuleEntity = byPermissionRuleName.get();
    Integer requiredPoints = pointPermissionRuleEntity.getRequiredPoints();
    if (requiredPoints <= 0) {
      return;
    }

    if (securityService.isAnonymous()) {
      throw new CustomException(i18nTool.getMessage("point.permissionRule.anonymous"));
    }

    handleActions(pointPermissionRuleEntity, dto);
  }

  /**
   * handle actions.
   *
   * @param pointPermissionRuleEntity pointPermissionRuleEntity
   * @param dto                       dto
   */
  private void handleActions(
      PointPermissionRuleEntity pointPermissionRuleEntity,
      PointPermissionRuleEventDto dto
  ) {
    PointEntity pointEntity = pointService.findPointByUserId(securityService.getUserId());
    int points = Math.abs(pointEntity.getPoints());
    int requiredPoints = Math.abs(pointPermissionRuleEntity.getRequiredPoints());
    if (points < requiredPoints) {
      throw new CustomException(
          i18nTool.getMessage(
              "point.permissionRule.points",
              Map.of(
                  "requiredPoints", requiredPoints,
                  "points", points
              )
          )
      );
    }

    pointCoreService.update(
        pointEntity,
        new UpdatePointDto(
            -Math.abs(pointPermissionRuleEntity.getOperationCost()),
            null,
            null
        )
    );

    String from = dto.from();
    String link = dto.link();

    getDifferenceSign(
        pointEntity,
        (flag, difference) -> {
          pointCoreService.create(
              pointEntity,
              difference,
              flag,
              null,
              dto.permissionRuleName(),
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
              userRepository.findById(securityService.getUserId())
                  .orElseThrow(UserNotFoundException::new)
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
