package com.youdeyiwu.event;

import org.springframework.context.ApplicationEvent;

/**
 * point permission rule event.
 *
 * @author dafengzhen
 */
public class PointPermissionRuleApplicationEvent extends ApplicationEvent {

  public PointPermissionRuleApplicationEvent(Object source) {
    super(source);
  }
}
