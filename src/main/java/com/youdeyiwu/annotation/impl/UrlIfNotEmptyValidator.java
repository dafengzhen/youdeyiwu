package com.youdeyiwu.annotation.impl;

import static com.youdeyiwu.tool.Tool.isValidLink;

import com.youdeyiwu.annotation.UrlIfNotEmpty;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import java.util.Objects;

/**
 * url if not empty.
 *
 * @author dafengzhen
 */
public class UrlIfNotEmptyValidator implements ConstraintValidator<UrlIfNotEmpty, String> {

  @Override
  public boolean isValid(String value, ConstraintValidatorContext context) {
    if (Objects.isNull(value) || value.trim().isEmpty()) {
      return true;
    }

    return isValidLink(value.toLowerCase());
  }
}
