package com.youdeyiwu.repository.config;

import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.entity.config.ConfigEntity;

/**
 * config.
 *
 * @author dafengzhen
 */
public interface CustomizedConfigRepository {

  /**
   * saveByTypeAndName.
   *
   * @param type  type
   * @param name  name
   * @param value value
   * @return ConfigEntity
   */
  ConfigEntity saveByTypeAndName(ConfigTypeEnum type, String name, String value);
}
