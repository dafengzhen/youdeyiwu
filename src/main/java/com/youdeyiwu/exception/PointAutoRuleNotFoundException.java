package com.youdeyiwu.exception;

/**
 * point rule not found.
 *
 * @author dafengzhen
 */
public class PointAutoRuleNotFoundException extends CustomException {
  public PointAutoRuleNotFoundException() {
    super("This point rule does not exist");
  }

  public PointAutoRuleNotFoundException(String message) {
    super(message);
  }
}
