package com.youdeyiwu.exception;

/**
 * point not found.
 *
 * @author dafengzhen
 */
public class PointNotFoundException extends CustomException {
  public PointNotFoundException() {
    super("This point does not exist");
  }

  public PointNotFoundException(String message) {
    super(message);
  }
}
