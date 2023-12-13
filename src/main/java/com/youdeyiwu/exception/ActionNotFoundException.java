package com.youdeyiwu.exception;

/**
 * action not found.
 *
 * @author dafengzhen
 */
public class ActionNotFoundException extends CustomException {
  public ActionNotFoundException() {
    super("This action does not exist");
  }

  public ActionNotFoundException(String message) {
    super(message);
  }
}
