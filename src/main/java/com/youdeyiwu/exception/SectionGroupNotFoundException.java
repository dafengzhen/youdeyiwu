package com.youdeyiwu.exception;

/**
 * section group not found.
 *
 * @author dafengzhen
 */
public class SectionGroupNotFoundException extends CustomException {
  public SectionGroupNotFoundException() {
    super("This section group does not exist");
  }

  public SectionGroupNotFoundException(String message) {
    super(message);
  }
}
