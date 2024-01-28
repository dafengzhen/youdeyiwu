package com.youdeyiwu.service.config;

import com.youdeyiwu.model.dto.config.UpdateJwtConfigDto;
import com.youdeyiwu.model.vo.config.JwtConfigVo;

/**
 * jwt.
 *
 * @author dafengzhen
 */
public interface JwtConfigService {

  /**
   * generate random secret.
   *
   * @return String
   */
  String generateRandomSecret();

  /**
   * query.
   *
   * @return JwtConfigVo
   */
  JwtConfigVo query();

  /**
   * update.
   *
   * @param dto dto
   */
  void update(UpdateJwtConfigDto dto);
}