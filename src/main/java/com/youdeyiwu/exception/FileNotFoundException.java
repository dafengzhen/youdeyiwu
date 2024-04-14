package com.youdeyiwu.exception;

/**
 * file not found.
 *
 * @author dafengzhen
 */
public class FileNotFoundException extends CustomException {
  public FileNotFoundException() {
    super("This file does not exist");
  }

  public FileNotFoundException(String message) {
    super(message);
  }
}
