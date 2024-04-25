package com.youdeyiwu.model.dto.data;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * create user.
 *
 * @param usernames       usernames
 * @param users           users
 * @param unifiedPassword unifiedPassword
 * @author dafengzhen
 */
public record CreateUserImportDto(
    Set<String> usernames,

    Set<UserImportDto> users,

    String unifiedPassword
) implements Serializable {

  /**
   * user.
   *
   * @param username username
   * @param password password
   */
  public record UserImportDto(
      String username,

      String password
  ) implements Serializable {

  }

  /**
   * user.
   *
   * @param id       id
   * @param username username
   */
  public record UserImportVo(
      Long id,

      String username
  ) implements Serializable {

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (o == null || getClass() != o.getClass()) {
        return false;
      }
      UserImportVo that = (UserImportVo) o;
      return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
      return Objects.hashCode(id);
    }
  }
}
