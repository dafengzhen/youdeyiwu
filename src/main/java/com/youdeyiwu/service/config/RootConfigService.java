package com.youdeyiwu.service.config;

import com.youdeyiwu.model.dto.config.UpdateRootConfigDto;

/**
 * root.
 *
 * @author dafengzhen
 */
public interface RootConfigService {

  /**
   * query disable registration.
   *
   * @return Boolean
   */
  Boolean queryDisableRegistration();

  /**
   * update.
   *
   * @param dto dto
   */
  void update(UpdateRootConfigDto dto);
}