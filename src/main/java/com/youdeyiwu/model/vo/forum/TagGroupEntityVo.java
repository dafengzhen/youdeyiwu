package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * tag group.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class TagGroupEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * sort.
   */
  private Integer sort;

  /**
   * tags.
   */
  private Set<TagEntityVo> tags;

}