package com.youdeyiwu.exception;

import org.springframework.http.HttpStatus;

/**
 * unauthorized.
 *
 * @author dafengzhen
 */
public class UnauthorizedException extends CustomException {
  public UnauthorizedException() {
    super(HttpStatus.UNAUTHORIZED.getReasonPhrase());
  }

  public UnauthorizedException(String message) {
    super(message);
  }
}
