package com.youdeyiwu.service.point.impl;

import static com.youdeyiwu.tool.Tool.getSign;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.exception.PointNotFoundException;
import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.point.PointRepository;
import com.youdeyiwu.service.point.PointCoreService;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
    userEntity.setPoint(entity);
    return pointRepository.save(entity);
  }

  @Transactional
  @Override
  public PointEntity update(PointEntity pointEntity, UpdatePointDto dto) {
    PointEntity entity = pointRepository.findById(pointEntity.getId())
        .orElseThrow(PointNotFoundException::new);
    entity.setOldPoints(entity.getPoints());

    if (Objects.nonNull(dto.points())) {
      getSign(dto.points(), sign -> {
        int points = Math.abs(dto.points());
        switch (sign) {
          case POSITIVE, ZERO -> entity.setPoints(entity.getPoints() + points);
          case NEGATIVE -> entity.setPoints(entity.getPoints() - points);
          default -> throw new IllegalStateException("Unexpected value: " + sign);
        }
      });
    }

    if (Objects.nonNull(dto.minPoints())) {
      entity.setMinPoints(dto.minPoints());
    }

    if (Objects.nonNull(dto.maxPoints())) {
      entity.setMaxPoints(dto.maxPoints());
    }

    if (
        entity.getPoints() < entity.getMinPoints()
            || entity.getPoints() > entity.getMaxPoints()
    ) {
      entity.setPoints(entity.getOldPoints());
    }

    return entity;
  }
}
