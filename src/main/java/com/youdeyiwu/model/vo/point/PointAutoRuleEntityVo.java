package com.youdeyiwu.model.vo.point;

import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * point auto rule.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PointAutoRuleEntityVo extends AbstractEntityVo {

  /**
   * auto rule name (The name of the rule used to automatically manage points, which can be considered passive).
   */
  private AutoRuleNameEnum autoRuleName;

  /**
   * required points.
   */
  private Integer requiredPoints;

}