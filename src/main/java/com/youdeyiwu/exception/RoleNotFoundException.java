package com.youdeyiwu.exception;

/**
 * role not found.
 *
 * @author dafengzhen
 */
public class RoleNotFoundException extends CustomException {
  public RoleNotFoundException() {
    super("This role does not exist");
  }

  public RoleNotFoundException(String message) {
    super(message);
  }
}
