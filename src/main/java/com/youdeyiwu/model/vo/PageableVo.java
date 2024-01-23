package com.youdeyiwu.model.vo;

import java.io.Serializable;
import lombok.Data;

/**
 * pageable.
 *
 * @author dafengzhen
 */
@Data
public class PageableVo implements Serializable {

  /**
   * page.
   */
  private Integer page;

  /**
   * size.
   */
  private Integer size;

  /**
   * previous.
   */
  private Boolean previous;

  /**
   * next.
   */
  private Boolean next;

  /**
   * pages.
   */
  private Integer pages;

  /**
   * elements.
   */
  private Long elements;

}