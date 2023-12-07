package com.youdeyiwu.config;

import com.youdeyiwu.auditing.SpringSecurityAuditorAware;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.security.SecurityService;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.domain.AuditorAware;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.web.config.EnableSpringDataWebSupport;

/**
 * config.
 *
 * @author dafengzhen
 */
@ConfigurationPropertiesScan
@EnableSpringDataWebSupport
@EnableJpaAuditing
@Import(ValidatorConfig.class)
@Configuration
public class Config {

  /**
   * auditorProvider.
   *
   * @return AuditorAware
   */
  @Bean
  public AuditorAware<UserEntity> auditorProvider(SecurityService securityService) {
    return new SpringSecurityAuditorAware(securityService);
  }
}
