package com.youdeyiwu.exception;

/**
 * post not found.
 *
 * @author dafengzhen
 */
public class PostNotFoundException extends CustomException {
  public PostNotFoundException() {
    super("This post does not exist");
  }

  public PostNotFoundException(String message) {
    super(message);
  }
}
