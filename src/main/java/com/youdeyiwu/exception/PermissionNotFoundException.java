package com.youdeyiwu.exception;

/**
 * permission not found.
 *
 * @author dafengzhen
 */
public class PermissionNotFoundException extends CustomException {
  public PermissionNotFoundException() {
    super("This permission does not exist");
  }

  public PermissionNotFoundException(String message) {
    super(message);
  }
}
