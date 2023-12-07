package com.youdeyiwu.exception;

import org.springframework.http.HttpStatus;

/**
 * too many requests.
 *
 * @author dafengzhen
 */
public class TooManyRequestsException extends CustomException {
  public TooManyRequestsException() {
    super(HttpStatus.TOO_MANY_REQUESTS.getReasonPhrase());
  }

  public TooManyRequestsException(String message) {
    super(message);
  }
}
