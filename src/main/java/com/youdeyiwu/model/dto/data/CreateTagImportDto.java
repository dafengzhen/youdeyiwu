package com.youdeyiwu.model.dto.data;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * create tag.
 *
 * @param tags tags
 * @author dafengzhen
 */
public record CreateTagImportDto(
    Set<TagImportDto> tags
) implements Serializable {

  /**
   * tag.
   *
   * @param name name
   */
  public record TagImportDto(
      String name
  ) implements Serializable {

  }

  /**
   * tag.
   *
   * @param id   id
   * @param name name
   */
  public record TagImportVo(
      Long id,

      String name
  ) implements Serializable {

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (o == null || getClass() != o.getClass()) {
        return false;
      }
      TagImportVo that = (TagImportVo) o;
      return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
      return Objects.hashCode(id);
    }
  }
}
