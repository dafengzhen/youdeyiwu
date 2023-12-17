package com.youdeyiwu.exception;

/**
 * message not found.
 *
 * @author dafengzhen
 */
public class MessageNotFoundException extends CustomException {
  public MessageNotFoundException() {
    super("This message does not exist");
  }

  public MessageNotFoundException(String message) {
    super(message);
  }
}
