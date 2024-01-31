package com.youdeyiwu.model.entity.point;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.ManyToOne;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * point history.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PointHistoryEntity extends AbstractEntity {

  /**
   * point Value.
   */
  private Integer pointValue = 0;

  /**
   * sign.
   */
  @Enumerated(EnumType.STRING)
  private SignEnum sign = SignEnum.ZERO;

  /**
   * points.
   */
  private Integer points = 0;

  /**
   * min points.
   */
  private Integer minPoints = 0;

  /**
   * max points.
   */
  private Integer maxPoints = 10000;

  /**
   * rule name (The name of the rule for manually managing points, which can be considered proactive).
   */
  @Enumerated(EnumType.STRING)
  private RuleNameEnum ruleName;

  /**
   * auto rule name (The name of the rule used to automatically manage points, which can be considered passive).
   */
  @Enumerated(EnumType.STRING)
  private AutoRuleNameEnum autoRuleName;

  /**
   * reason.
   */
  private String reason;

  /**
   * user.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity user;

}