package com.youdeyiwu.model.vo;

import lombok.Data;

/**
 * error.
 *
 * @author dafengzhen
 */
@Data
public class ErrorVo {

  /**
   * status.
   */
  private Integer status;
  /**
   * message.
   */
  private String message;

  /**
   * error.
   *
   * @param status  status
   * @param message message
   */
  public ErrorVo(Integer status, String message) {
    this.status = status;
    this.message = message;
  }

}
