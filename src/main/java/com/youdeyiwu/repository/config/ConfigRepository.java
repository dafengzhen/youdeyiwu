package com.youdeyiwu.repository.config;

import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import java.util.Optional;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * config.
 *
 * @author dafengzhen
 */
public interface ConfigRepository extends JpaRepositoryImplementation<ConfigEntity, Long>, CustomizedConfigRepository {

  /**
   * findByTypeAndName.
   *
   * @param type type
   * @param name name
   * @return ConfigEntity
   */
  ConfigEntity findByTypeAndName(ConfigTypeEnum type, String name);

  /**
   * findOptionalByTypeAndName.
   *
   * @param type type
   * @param name name
   * @return ConfigEntity
   */
  Optional<ConfigEntity> findOptionalByTypeAndName(ConfigTypeEnum type, String name);
}
