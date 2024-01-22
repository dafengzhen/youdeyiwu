package com.youdeyiwu.event;

import org.springframework.context.ApplicationEvent;

/**
 * post review state event.
 *
 * @author dafengzhen
 */
public class PostReviewStateApplicationEvent extends ApplicationEvent {

  public PostReviewStateApplicationEvent(Object source) {
    super(source);
  }
}
