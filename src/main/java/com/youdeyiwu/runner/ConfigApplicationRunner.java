package com.youdeyiwu.runner;

import static com.youdeyiwu.tool.JwtTool.encodeSecret;
import static com.youdeyiwu.tool.Tool.randomUuId;

import com.youdeyiwu.constant.JwtConfigConstant;
import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.constant.RootConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
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
    initRootSecretConfig();
    initJwtSecretConfig();
    initPointConfig();
    log.info("=== Config === Initial configuration completed");
  }

  /**
   * init jwt secret config.
   */
  private void initJwtSecretConfig() {
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
      configEntity.setValue(encodeSecret());
      configRepository.save(configEntity);
      log.info("=== Config === Create jwt.secret option");
    }
  }

  /**
   * init root secret config.
   */
  private void initRootSecretConfig() {
    if (
        Optional.ofNullable(
                configRepository.findByTypeAndName(
                    ConfigTypeEnum.ROOT,
                    RootConfigConstant.SECRET
                )
            )
            .isEmpty()
    ) {
      ConfigEntity configEntity = new ConfigEntity();
      configEntity.setType(ConfigTypeEnum.ROOT);
      configEntity.setName(RootConfigConstant.SECRET);
      configEntity.setValue(randomUuId());
      configRepository.save(configEntity);
      log.info(
          """
              === Config === Create root.secret option
              === The secret will only be initialized once
              === and then stored in the database as credentials to set up the initial administrator for the forum
              === Please do not disclose
              === {}
              """,
          configEntity.getValue()
      );
    }
  }

  /**
   * init point config.
   */
  private void initPointConfig() {
    if (
        Optional.ofNullable(
                configRepository.findByTypeAndName(
                    ConfigTypeEnum.POINT,
                    PointConfigConstant.ENABLE
                )
            )
            .isEmpty()
    ) {
      ConfigEntity configEntity = new ConfigEntity();
      configEntity.setType(ConfigTypeEnum.POINT);
      configEntity.setName(PointConfigConstant.ENABLE);
      configEntity.setValue("false");
      configRepository.save(configEntity);
      log.info("=== Config === Create point.enable option");
    }

    if (
        Optional.ofNullable(
                configRepository.findByTypeAndName(
                    ConfigTypeEnum.POINT,
                    PointConfigConstant.INIT_POINTS
                )
            )
            .isEmpty()
    ) {
      ConfigEntity configEntity = new ConfigEntity();
      configEntity.setType(ConfigTypeEnum.POINT);
      configEntity.setName(PointConfigConstant.INIT_POINTS);
      configEntity.setValue("100");
      configRepository.save(configEntity);
      log.info("=== Config === Create point.initPoints option");
    }
  }
}