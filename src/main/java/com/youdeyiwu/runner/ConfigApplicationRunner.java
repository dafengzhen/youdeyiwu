package com.youdeyiwu.runner;

import com.youdeyiwu.constant.JwtConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.tool.JwtTool;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * config.
 *
 * @author dafengzhen
 */
@Log4j2
@Order(1)
@RequiredArgsConstructor
@Component
public class ConfigApplicationRunner implements ApplicationRunner {

  private final ConfigRepository configRepository;

  @Transactional
  @Override
  public void run(ApplicationArguments args) throws Exception {
    if (
        Optional.ofNullable(
                configRepository.findByTypeAndName(
                    ConfigTypeEnum.JWT,
                    JwtConfigConstant.SECRET
                )
            )
            .isEmpty()
    ) {
      ConfigEntity configEntity = new ConfigEntity();
      configEntity.setType(ConfigTypeEnum.JWT);
      configEntity.setName(JwtConfigConstant.SECRET);
      configEntity.setValue(JwtTool.encodeSecret());
      configRepository.save(configEntity);
      log.info("=== Config === Create jwt.secret option");
      log.info("=== Config === Initial configuration completed");
    }
  }

}