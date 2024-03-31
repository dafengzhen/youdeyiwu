package com.youdeyiwu.model.vo.point;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * point permission rule.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PointPermissionRuleEntityVo extends AbstractEntityVo {

  /**
   * permission rule name.
   */
  private PermissionRuleNameEnum permissionRuleName;

  /**
   * required points.
   */
  private Integer requiredPoints;

  /**
   * operation cost (points deducted from user's points when the required points are met).
   */
  private Integer operationCost;

  /**
   * enable.
   */
  private Boolean enable;

}