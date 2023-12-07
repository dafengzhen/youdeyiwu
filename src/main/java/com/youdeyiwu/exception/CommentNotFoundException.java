package com.youdeyiwu.exception;

/**
 * comment not found.
 *
 * @author dafengzhen
 */
public class CommentNotFoundException extends CustomException {
  public CommentNotFoundException() {
    super("This comment does not exist");
  }

  public CommentNotFoundException(String message) {
    super(message);
  }
}
