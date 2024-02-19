package com.youdeyiwu.config;

import jakarta.validation.Configuration;
import org.hibernate.validator.HibernateValidatorConfiguration;
import org.springframework.boot.autoconfigure.validation.ValidationConfigurationCustomizer;

/**
 * validator config.
 *
 * @author dafengzhen
 */
public class ValidatorConfig implements ValidationConfigurationCustomizer {

  @Override
  public void customize(Configuration<?> configuration) {
    HibernateValidatorConfiguration hibernateValidatorConfiguration =
        (HibernateValidatorConfiguration) configuration;
    hibernateValidatorConfiguration.failFast(true);
  }
}
