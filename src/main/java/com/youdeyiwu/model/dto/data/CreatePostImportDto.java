package com.youdeyiwu.model.dto.data;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * create post.
 *
 * @param posts posts
 * @author dafengzhen
 */
public record CreatePostImportDto(
    Set<PostImportDto> posts
) implements Serializable {

  /**
   * post.
   *
   * @param name        name
   * @param overview    overview
   * @param content     content
   * @param contentLink contentLink
   * @param styles      styles
   * @param classNames  classNames
   * @param sections    sections
   * @param tags        tags
   * @param createdOn   createdOn
   * @param createdBy   createdBy
   */
  public record PostImportDto(
      String name,

      String overview,

      String content,

      String contentLink,

      String styles,

      String classNames,

      CreateSectionImportDto sections,

      CreateTagImportDto tags,

      String createdOn,

      String createdBy
  ) implements Serializable {

  }

  /**
   * post.
   *
   * @param id   id
   * @param name name
   */
  public record PostImportVo(
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
      PostImportVo that = (PostImportVo) o;
      return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
      return Objects.hashCode(id);
    }
  }
}
