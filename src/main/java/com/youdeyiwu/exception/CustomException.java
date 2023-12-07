package com.youdeyiwu.exception;

/**
 * custom exception.
 *
 * @author dafengzhen
 */
public class CustomException extends RuntimeException {
  public CustomException() {
    super("Sorry, an unknown error occurred");
  }

  public CustomException(String message) {
    super(message);
  }
}
