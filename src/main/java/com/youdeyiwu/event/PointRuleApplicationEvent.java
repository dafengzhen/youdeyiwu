package com.youdeyiwu.event;

import org.springframework.context.ApplicationEvent;

/**
 * point rule event.
 *
 * @author dafengzhen
 */
public class PointRuleApplicationEvent extends ApplicationEvent {

  public PointRuleApplicationEvent(Object source) {
    super(source);
  }
}
