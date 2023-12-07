package com.youdeyiwu.exception;

/**
 * tag group not found.
 *
 * @author dafengzhen
 */
public class TagGroupNotFoundException extends CustomException {
  public TagGroupNotFoundException() {
    super("This tag group does not exist");
  }

  public TagGroupNotFoundException(String message) {
    super(message);
  }
}
