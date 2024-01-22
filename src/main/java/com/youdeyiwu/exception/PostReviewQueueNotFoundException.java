package com.youdeyiwu.exception;

/**
 * post review queue not found.
 *
 * @author dafengzhen
 */
public class PostReviewQueueNotFoundException extends CustomException {
  public PostReviewQueueNotFoundException() {
    super("This post review queue does not exist");
  }

  public PostReviewQueueNotFoundException(String message) {
    super(message);
  }
}
