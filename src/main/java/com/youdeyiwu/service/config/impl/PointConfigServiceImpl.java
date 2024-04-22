package com.youdeyiwu.service.config.impl;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.dto.config.UpdatePointConfigDto;
import com.youdeyiwu.model.vo.config.PointConfigVo;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.service.config.PointConfigService;
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
public class PointConfigServiceImpl implements PointConfigService {

  private final ConfigRepository configRepository;

  @Override
  public PointConfigVo query() {
    Boolean enable = configRepository.findOptionalByTypeAndName(
            ConfigTypeEnum.POINT,
            PointConfigConstant.ENABLE
        )
        .map(configEntity -> Boolean.valueOf(configEntity.getValue()))
        .orElse(false);

    Integer initPoints = configRepository.findOptionalByTypeAndName(
            ConfigTypeEnum.POINT,
            PointConfigConstant.INIT_POINTS
        )
        .map(configEntity -> Integer.valueOf(configEntity.getValue()))
        .orElse(100);

    PointConfigVo vo = new PointConfigVo();
    vo.setEnable(enable);
    vo.setInitPoints(initPoints);
    return vo;
  }

  @Transactional
  @Override
  public void update(UpdatePointConfigDto dto) {
    if (Objects.nonNull(dto.enable())) {
      configRepository.saveByTypeAndName(
          ConfigTypeEnum.POINT,
          PointConfigConstant.ENABLE,
          String.valueOf(dto.enable())
      );
    }

    if (Objects.nonNull(dto.initPoints())) {
      configRepository.saveByTypeAndName(
          ConfigTypeEnum.POINT,
          PointConfigConstant.INIT_POINTS,
          String.valueOf(dto.initPoints())
      );
    }
  }
}