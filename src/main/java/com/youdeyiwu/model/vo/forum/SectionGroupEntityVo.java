package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * section group.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class SectionGroupEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * sort.
   */
  private Integer sort;

  /**
   * sections.
   */
  private Set<SectionEntityVo> sections;

}