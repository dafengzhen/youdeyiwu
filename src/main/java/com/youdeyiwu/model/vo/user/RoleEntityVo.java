package com.youdeyiwu.model.vo.user;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * role.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class RoleEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * overview.
   */
  private String overview;

  /**
   * sort.
   */
  private Integer sort;

  /**
   * display.
   */
  private Boolean display;

  /**
   * permissions.
   */
  private Set<PermissionEntityVo> permissions;

}