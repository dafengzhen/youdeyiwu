package com.youdeyiwu.model.vo;

import java.io.Serializable;
import lombok.Data;

/**
 * token.
 *
 * @author dafengzhen
 */
@Data
public class TokenVo implements Serializable {

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
