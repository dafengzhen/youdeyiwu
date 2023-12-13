package com.youdeyiwu.exception;

/**
 * submenu not found.
 *
 * @author dafengzhen
 */
public class SubmenuNotFoundException extends CustomException {
  public SubmenuNotFoundException() {
    super("This submenu does not exist");
  }

  public SubmenuNotFoundException(String message) {
    super(message);
  }
}
