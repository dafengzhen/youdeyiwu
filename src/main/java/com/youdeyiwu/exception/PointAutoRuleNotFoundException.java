package com.youdeyiwu.exception;

/**
 * point auto rule not found.
 *
 * @author dafengzhen
 */
public class PointAutoRuleNotFoundException extends CustomException {
  public PointAutoRuleNotFoundException() {
    super("This point auto rule does not exist");
  }

  public PointAutoRuleNotFoundException(String message) {
    super(message);
  }
}
