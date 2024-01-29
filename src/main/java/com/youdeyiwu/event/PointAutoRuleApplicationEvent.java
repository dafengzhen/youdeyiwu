package com.youdeyiwu.event;

import org.springframework.context.ApplicationEvent;

/**
 * point auto rule event.
 *
 * @author dafengzhen
 */
public class PointAutoRuleApplicationEvent extends ApplicationEvent {

  public PointAutoRuleApplicationEvent(Object source) {
    super(source);
  }
}
