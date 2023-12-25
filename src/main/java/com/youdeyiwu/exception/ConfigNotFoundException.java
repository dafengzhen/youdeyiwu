package com.youdeyiwu.exception;

/**
 * config not found.
 *
 * @author dafengzhen
 */
public class ConfigNotFoundException extends CustomException {
  public ConfigNotFoundException() {
    super("This config does not exist");
  }

  public ConfigNotFoundException(String message) {
    super(message);
  }
}
