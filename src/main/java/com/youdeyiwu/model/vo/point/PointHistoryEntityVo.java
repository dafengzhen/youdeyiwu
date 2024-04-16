package com.youdeyiwu.model.vo.point;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
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
   * rule name.
   */
  private RuleNameEnum ruleName;

  /**
   * permission rule name.
   */
  private PermissionRuleNameEnum permissionRuleName;

  /**
   * reason.
   */
  private String reason;

  /**
   * source.
   */
  private String source;

  /**
   * source link.
   */
  private String sourceLink;

  /**
   * user.
   */
  private UserEntityVo user;

}