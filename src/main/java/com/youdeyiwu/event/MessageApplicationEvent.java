package com.youdeyiwu.event;

import org.springframework.context.ApplicationEvent;

/**
 * message event.
 *
 * @author dafengzhen
 */
public class MessageApplicationEvent extends ApplicationEvent {

  public MessageApplicationEvent(Object source) {
    super(source);
  }
}
