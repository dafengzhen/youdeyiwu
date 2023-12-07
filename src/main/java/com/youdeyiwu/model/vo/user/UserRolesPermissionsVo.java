package com.youdeyiwu.model.vo.user;

import java.io.Serializable;
import java.util.List;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;

/**
 * user roles permissions.
 *
 * @author dafengzhen
 */
@Getter
@Setter
public class UserRolesPermissionsVo implements Serializable {

  /**
   * user.
   */
  private UserEntityVo user;

  /**
   * roles.
   */
  private List<RoleEntityVo> roles;

  /**
   * permissions.
   */
  private Set<PermissionEntityVo> permissions;

}
