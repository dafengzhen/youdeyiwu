package com.youdeyiwu.service.point.impl;

import static com.youdeyiwu.tool.Tool.getSign;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.exception.PointNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.point.PointMapper;
import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.point.PointRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.service.point.PointCoreService;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * point.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class PointCoreServiceImpl implements PointCoreService {

  private final PointRepository pointRepository;

  private final ConfigRepository configRepository;

  private final UserRepository userRepository;

  private final PointMapper pointMapper;

  @Transactional
  @Override
  public PointEntity create(UserEntity userEntity) {
    ConfigEntity initPoints = configRepository.findByTypeAndName(
        ConfigTypeEnum.POINT,
        PointConfigConstant.INIT_POINTS
    );

    PointEntity entity = new PointEntity();
    entity.setPoints(Integer.valueOf(initPoints.getValue()));
    entity.setUser(userEntity);
    userEntity.setPoint(pointRepository.save(entity));
    return userEntity.getPoint();
  }

  @Transactional
  @Override
  public void create(
      PointEntity pointEntity,
      Integer pointValue,
      SignEnum sign,
      RuleNameEnum ruleName,
      PermissionRuleNameEnum permissionRuleName,
      String reason
  ) {
    PointHistoryEntity pointHistoryEntity = pointMapper.entityToEntity(pointEntity);
    if (Objects.nonNull(pointValue)) {
      pointHistoryEntity.setPointValue(pointValue);
    }

    if (Objects.nonNull(sign)) {
      pointHistoryEntity.setSign(sign);
    }

    if (Objects.nonNull(ruleName)) {
      pointHistoryEntity.setRuleName(ruleName);
    }

    if (Objects.nonNull(permissionRuleName)) {
      pointHistoryEntity.setPermissionRuleName(permissionRuleName);
    }

    if (StringUtils.hasText(reason)) {
      pointHistoryEntity.setReason(reason);
    }

    UserEntity userEntity = userRepository.findById(pointEntity.getUser().getId())
        .orElseThrow(UserNotFoundException::new);
    pointHistoryEntity.setUser(userEntity);
    userEntity.getPointHistories().add(pointHistoryEntity);
  }

  @Transactional
  @Override
  public PointEntity update(PointEntity pointEntity, UpdatePointDto dto) {
    PointEntity entity = pointRepository.findById(pointEntity.getId())
        .orElseThrow(PointNotFoundException::new);
    entity.setOldPoints(entity.getPoints());

    if (Objects.nonNull(dto.minPoints())) {
      entity.setMinPoints(dto.minPoints());
    }

    if (Objects.nonNull(dto.maxPoints())) {
      entity.setMaxPoints(dto.maxPoints());
    }

    if (Objects.nonNull(dto.points())) {
      getSign(dto.points(), sign -> {
        int points = Math.abs(dto.points());
        switch (sign) {
          case POSITIVE, ZERO -> {
            if (entity.getPoints() > entity.getMaxPoints()) {
              return;
            }
            entity.setPoints(entity.getPoints() + points);
          }
          case NEGATIVE -> {
            if (entity.getPoints() < entity.getMinPoints()) {
              return;
            }
            entity.setPoints(entity.getPoints() - points);
          }
          default -> throw new IllegalStateException("Unexpected value: " + sign);
        }
      });
    }

    return entity;
  }
}
