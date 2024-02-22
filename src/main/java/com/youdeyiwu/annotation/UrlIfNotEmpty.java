package com.youdeyiwu.annotation;

import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.ElementType.TYPE_USE;

import com.youdeyiwu.annotation.impl.UrlIfNotEmptyValidator;
import jakarta.validation.Constraint;
import jakarta.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * url if not empty.
 *
 * @author dafengzhen
 */
@Documented
@Constraint(validatedBy = UrlIfNotEmptyValidator.class)
@Target({ METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE })
@Retention(RetentionPolicy.RUNTIME)
public @interface UrlIfNotEmpty {

  /**
   * message.
   *
   * @return String
   */
  String message() default "{validator.url.invalid}";

  /**
   * groups.
   *
   * @return Class
   */
  Class<?>[] groups() default {};

  /**
   * payload.
   *
   * @return Class
   */
  Class<? extends Payload>[] payload() default {};

  /**
   * List.
   */
  @Target({ METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE })
  @Retention(RetentionPolicy.RUNTIME)
  @Documented
  @interface List {
    /**
     * value.
     *
     * @return UrlIfNotEmpty
     */
    UrlIfNotEmpty[] value();
  }
}