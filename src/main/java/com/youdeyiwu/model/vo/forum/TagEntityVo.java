package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * tag.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class TagEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * sort.
   */
  private Integer sort;

}