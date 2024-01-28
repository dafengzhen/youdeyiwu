package com.youdeyiwu.service.config;

import com.youdeyiwu.model.dto.config.UpdatePointConfigDto;
import com.youdeyiwu.model.vo.config.PointConfigVo;

/**
 * point.
 *
 * @author dafengzhen
 */
public interface PointConfigService {

  /**
   * query.
   *
   * @return PointConfigVo
   */
  PointConfigVo query();

  /**
   * update.
   *
   * @param dto dto
   */
  void update(UpdatePointConfigDto dto);
}