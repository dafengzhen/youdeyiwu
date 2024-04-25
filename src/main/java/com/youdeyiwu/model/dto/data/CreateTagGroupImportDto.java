package com.youdeyiwu.model.dto.data;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * create tag group.
 *
 * @param tagGroups tagGroups
 * @author dafengzhen
 */
public record CreateTagGroupImportDto(
    Set<TagGroupImportDto> tagGroups
) implements Serializable {

  /**
   * tag group.
   *
   * @param name name
   * @param tags tags
   */
  public record TagGroupImportDto(
      String name,

      CreateTagImportDto tags
  ) implements Serializable {

  }

  /**
   * tag group.
   *
   * @param id   id
   * @param name name
   */
  public record TagGroupImportVo(
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
      TagGroupImportVo that = (TagGroupImportVo) o;
      return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
      return Objects.hashCode(id);
    }
  }
}
