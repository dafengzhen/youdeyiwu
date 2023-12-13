package com.youdeyiwu.exception;

/**
 * menu not found.
 *
 * @author dafengzhen
 */
public class MenuNotFoundException extends CustomException {
  public MenuNotFoundException() {
    super("This menu does not exist");
  }

  public MenuNotFoundException(String message) {
    super(message);
  }
}
