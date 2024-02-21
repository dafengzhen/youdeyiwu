package com.youdeyiwu.mapper.other;

import com.youdeyiwu.config.MapperTemplateConfig;
import java.util.Objects;
import org.mapstruct.Mapper;
import org.mapstruct.Named;

/**
 * string.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface StringMapper {

  /**
   * trim.
   *
   * @param value value
   * @return String
   */
  @Named("trim")
  default String trim(String value) {
    return value.trim();
  }

  /**
   * trimIfNotNull.
   *
   * @param value value
   * @return String
   */
  @Named("trimIfNotNull")
  default String trimIfNotNull(String value) {
    if (Objects.nonNull(value)) {
      return trim(value);
    }
    return null;
  }
}
