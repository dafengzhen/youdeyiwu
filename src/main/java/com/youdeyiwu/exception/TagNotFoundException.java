package com.youdeyiwu.exception;

/**
 * tag not found.
 *
 * @author dafengzhen
 */
public class TagNotFoundException extends CustomException {
  public TagNotFoundException() {
    super("This tag does not exist");
  }

  public TagNotFoundException(String message) {
    super(message);
  }
}
