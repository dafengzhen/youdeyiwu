package com.youdeyiwu.exception;

/**
 * section not found.
 *
 * @author dafengzhen
 */
public class SectionNotFoundException extends CustomException {
  public SectionNotFoundException() {
    super("This section does not exist");
  }

  public SectionNotFoundException(String message) {
    super(message);
  }
}
