package com.youdeyiwu.repository.config;

import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import org.springframework.data.repository.CrudRepository;

/**
 * config.
 *
 * @author dafengzhen
 */
public interface ConfigRepository extends CrudRepository<ConfigEntity, Long> {

  /**
   * findByName.
   *
   * @param type type
   * @param name name
   * @return ConfigEntity
   */
  ConfigEntity findByTypeAndName(ConfigTypeEnum type, String name);

}
