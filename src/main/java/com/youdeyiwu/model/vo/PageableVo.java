package com.youdeyiwu.model.vo;

import lombok.Data;

/**
 * pageable.
 *
 * @author dafengzhen
 */
@Data
public class PageableVo {

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