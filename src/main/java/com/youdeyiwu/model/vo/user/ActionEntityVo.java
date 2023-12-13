package com.youdeyiwu.model.vo.user;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * action.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class ActionEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * alias (The alias defaults to the name).
   */
  private String alias;

  /**
   * sort.
   */
  private Integer sort;

  /**
   * menu.
   */
  private MenuEntityVo menu;

  /**
   * submenu.
   */
  private SubmenuEntityVo submenu;

}