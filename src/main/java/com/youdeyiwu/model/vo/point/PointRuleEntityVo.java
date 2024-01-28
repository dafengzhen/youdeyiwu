package com.youdeyiwu.model.vo.point;

import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * point rule.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PointRuleEntityVo extends AbstractEntityVo {

  /**
   * rule name (The name of the rule for manually managing points, which can be considered proactive).
   */
  private RuleNameEnum ruleName;

  /**
   * required points.
   */
  private Integer requiredPoints;

}