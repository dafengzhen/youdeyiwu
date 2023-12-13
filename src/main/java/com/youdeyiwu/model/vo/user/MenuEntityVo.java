package com.youdeyiwu.model.vo.user;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * menu.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class MenuEntityVo extends AbstractEntityVo {

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
   * submenus.
   */
  private Set<SubmenuEntityVo> submenus;

}