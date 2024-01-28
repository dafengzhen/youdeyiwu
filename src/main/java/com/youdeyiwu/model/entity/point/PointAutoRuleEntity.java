package com.youdeyiwu.model.entity.point;

import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * point auto rule.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PointAutoRuleEntity extends AbstractEntity {

  /**
   * auto rule name (The name of the rule used to automatically manage points, which can be considered passive).
   */
  @Enumerated(EnumType.STRING)
  @Column(unique = true)
  private AutoRuleNameEnum autoRuleName;

  /**
   * required points.
   */
  private Integer requiredPoints = 0;

}