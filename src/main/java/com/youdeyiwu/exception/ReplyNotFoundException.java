package com.youdeyiwu.exception;

/**
 * reply not found.
 *
 * @author dafengzhen
 */
public class ReplyNotFoundException extends CustomException {
  public ReplyNotFoundException() {
    super("This reply does not exist");
  }

  public ReplyNotFoundException(String message) {
    super(message);
  }
}
