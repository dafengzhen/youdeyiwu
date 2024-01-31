package com.youdeyiwu.model.vo.point;

import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * point history.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PointHistoryEntityVo extends AbstractEntityVo {

  /**
   * point Value.
   */
  private Integer pointValue;

  /**
   * sign.
   */
  private SignEnum sign;

  /**
   * points.
   */
  private Integer points;

  /**
   * min points.
   */
  private Integer minPoints;

  /**
   * max points.
   */
  private Integer maxPoints;

  /**
   * rule name (The name of the rule for manually managing points, which can be considered proactive).
   */
  private RuleNameEnum ruleName;

  /**
   * auto rule name (The name of the rule used to automatically manage points, which can be considered passive).
   */
  private AutoRuleNameEnum autoRuleName;

  /**
   * reason.
   */
  private String reason;

  /**
   * user.
   */
  private UserEntityVo user;

}