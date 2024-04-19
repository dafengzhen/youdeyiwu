package com.youdeyiwu.service.config;

import com.youdeyiwu.model.dto.config.UpdateRootConfigDto;
import com.youdeyiwu.model.vo.config.RootConfigVo;

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
   * query.
   *
   * @param disableRegistration disableRegistration
   * @param disableAnonymous    disableAnonymous
   * @return RootConfigVo
   */
  RootConfigVo query(Boolean disableRegistration, Boolean disableAnonymous);

  /**
   * update.
   *
   * @param dto dto
   */
  void update(UpdateRootConfigDto dto);
}