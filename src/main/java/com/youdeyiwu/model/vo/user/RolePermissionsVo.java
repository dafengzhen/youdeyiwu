package com.youdeyiwu.model.vo.user;

import java.io.Serializable;
import java.util.List;
import lombok.Getter;
import lombok.Setter;

/**
 * role permissions.
 *
 * @author dafengzhen
 */
@Getter
@Setter
public class RolePermissionsVo implements Serializable {

  private RoleEntityVo role;

  private List<PermissionEntityVo> permissions;

}
