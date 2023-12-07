package com.youdeyiwu.model.vo;

import lombok.Data;

/**
 * token.
 *
 * @author dafengzhen
 */
@Data
public class TokenVo {

  /**
   * id.
   */
  private Long id;

  /**
   * alias.
   */
  private String alias;

  /**
   * token.
   */
  private String token;

  /**
   * expDays.
   */
  private Integer expDays;

}
