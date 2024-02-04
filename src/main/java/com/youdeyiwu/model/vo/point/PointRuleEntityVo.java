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
   * rule name.
   */
  private RuleNameEnum ruleName;

  /**
   * points awarded to the initiator user.
   */
  private Integer initiatorRewardPoints;

  /**
   * points awarded to the receiver user.
   */
  private Integer receiverRewardPoints;

}