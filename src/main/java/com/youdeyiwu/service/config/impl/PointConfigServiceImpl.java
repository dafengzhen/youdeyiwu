package com.youdeyiwu.service.config.impl;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.dto.config.UpdatePointConfigDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
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
    ConfigEntity enable = configRepository.findByTypeAndName(
        ConfigTypeEnum.POINT,
        PointConfigConstant.ENABLE
    );
    ConfigEntity initPoints = configRepository.findByTypeAndName(
        ConfigTypeEnum.POINT,
        PointConfigConstant.INIT_POINTS
    );

    PointConfigVo vo = new PointConfigVo();
    vo.setEnable(Boolean.valueOf(enable.getValue()));
    vo.setInitPoints(Integer.valueOf(initPoints.getValue()));
    return vo;
  }

  @Transactional
  @Override
  public void update(UpdatePointConfigDto dto) {
    if (Objects.nonNull(dto.enable())) {
      configRepository.findByTypeAndName(
              ConfigTypeEnum.POINT,
              PointConfigConstant.ENABLE
          )
          .setValue(String.valueOf(dto.enable()));
    }

    if (Objects.nonNull(dto.initPoints())) {
      configRepository.findByTypeAndName(
              ConfigTypeEnum.POINT,
              PointConfigConstant.INIT_POINTS
          )
          .setValue(String.valueOf(dto.initPoints()));
    }
  }
}