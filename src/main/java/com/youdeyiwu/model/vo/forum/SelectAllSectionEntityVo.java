package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * select all.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class SelectAllSectionEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

}