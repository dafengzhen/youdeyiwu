package com.youdeyiwu.model.vo.user;

import com.youdeyiwu.model.vo.AbstractEntityVo;
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

}