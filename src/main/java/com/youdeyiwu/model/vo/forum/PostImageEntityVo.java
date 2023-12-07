package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * post image.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PostImageEntityVo extends AbstractEntityVo {

  /**
   * url.
   */
  private String url;

  /**
   * sort.
   */
  private Integer sort;

}