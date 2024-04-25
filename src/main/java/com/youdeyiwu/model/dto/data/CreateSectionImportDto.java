package com.youdeyiwu.model.dto.data;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * create section.
 *
 * @param sections sections
 * @author dafengzhen
 */
public record CreateSectionImportDto(
    Set<SectionImportDto> sections
) implements Serializable {

  /**
   * section.
   *
   * @param name          name
   * @param overview      overview
   * @param content       content
   * @param admins        admins
   * @param tags          tags
   * @param sectionGroups sectionGroups
   */
  public record SectionImportDto(
      String name,

      String overview,

      String content,

      CreateUserImportDto admins,

      CreateTagImportDto tags,

      CreateSectionGroupImportDto sectionGroups
  ) implements Serializable {

  }

  /**
   * section.
   *
   * @param id   id
   * @param name name
   */
  public record SectionImportVo(
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
      SectionImportVo that = (SectionImportVo) o;
      return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
      return Objects.hashCode(id);
    }
  }
}
