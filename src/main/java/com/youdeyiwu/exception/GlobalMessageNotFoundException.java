package com.youdeyiwu.exception;

/**
 * global message not found.
 *
 * @author dafengzhen
 */
public class GlobalMessageNotFoundException extends CustomException {
  public GlobalMessageNotFoundException() {
    super("This global message does not exist");
  }

  public GlobalMessageNotFoundException(String message) {
    super(message);
  }
}
