package com.youdeyiwu.listener;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.event.PointPermissionRuleApplicationEvent;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.model.dto.point.PointPermissionRuleEventDto;
import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointPermissionRuleEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.point.PointPermissionRuleRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.point.PointCoreService;
import com.youdeyiwu.service.point.PointService;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
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
      throw new CustomException(
          """
              Sorry, the system's points mechanism has been enabled.
              Unauthenticated users are unable to access this resource
              """
      );
    }

    handleActions(pointPermissionRuleEntity);
  }

  /**
   * handle actions.
   *
   * @param pointPermissionRuleEntity pointPermissionRuleEntity
   */
  private void handleActions(
      PointPermissionRuleEntity pointPermissionRuleEntity
  ) {
    PointEntity pointEntity = pointService.findPointByUserId(securityService.getUserId());
    int points = Math.abs(pointEntity.getPoints());
    int requiredPoints = Math.abs(pointPermissionRuleEntity.getRequiredPoints());
    if (points < requiredPoints) {
      throw new CustomException(
          """
              Sorry, you do not have sufficient privileges to access this resource.
              The required points are %s,
              and your current points are %s
               """
              .formatted(requiredPoints, points)
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
  }
}
