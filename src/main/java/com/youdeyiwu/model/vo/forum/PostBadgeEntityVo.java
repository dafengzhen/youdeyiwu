package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * post badge.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PostBadgeEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * sort.
   */
  private Integer sort;

  /**
   * styles.
   */
  private String styles;

  /**
   * classes.
   */
  private String classes;

}