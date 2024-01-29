package com.youdeyiwu.exception;

/**
 * point rule not found.
 *
 * @author dafengzhen
 */
public class PointRuleNotFoundException extends CustomException {
  public PointRuleNotFoundException() {
    super("This point rule does not exist");
  }

  public PointRuleNotFoundException(String message) {
    super(message);
  }
}
