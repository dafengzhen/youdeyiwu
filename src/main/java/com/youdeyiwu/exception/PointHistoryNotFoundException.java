package com.youdeyiwu.exception;

/**
 * point history not found.
 *
 * @author dafengzhen
 */
public class PointHistoryNotFoundException extends CustomException {
  public PointHistoryNotFoundException() {
    super("This point history does not exist");
  }

  public PointHistoryNotFoundException(String message) {
    super(message);
  }
}
