package com.youdeyiwu.exception;

/**
 * user not found.
 *
 * @author dafengzhen
 */
public class UserNotFoundException extends CustomException {
  public UserNotFoundException() {
    super("This user does not exist");
  }

  public UserNotFoundException(String message) {
    super(message);
  }
}
