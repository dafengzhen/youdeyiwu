package com.youdeyiwu.model.dto.data;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * create section group.
 *
 * @param sectionGroups sectionGroups
 * @author dafengzhen
 */
public record CreateSectionGroupImportDto(
    Set<SectionGroupImportDto> sectionGroups
) implements Serializable {

  /**
   * section group.
   *
   * @param name     name
   * @param sections sections
   */
  public record SectionGroupImportDto(
      String name,

      CreateSectionImportDto sections
  ) implements Serializable {

  }

  /**
   * section group.
   *
   * @param id   id
   * @param name name
   */
  public record SectionGroupImportVo(
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
      SectionGroupImportVo that = (SectionGroupImportVo) o;
      return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
      return Objects.hashCode(id);
    }
  }
}
