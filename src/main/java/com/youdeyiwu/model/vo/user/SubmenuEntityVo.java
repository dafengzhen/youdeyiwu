package com.youdeyiwu.model.vo.user;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * submenu.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class SubmenuEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * link.
   */
  private String link;

  /**
   * sort.
   */
  private Integer sort;

  /**
   * menu.
   */
  private MenuEntityVo menu;

  /**
   * actions.
   */
  private Set<ActionEntityVo> actions;

  /**
   * roles.
   */
  private Set<RoleEntityVo> roles;

}