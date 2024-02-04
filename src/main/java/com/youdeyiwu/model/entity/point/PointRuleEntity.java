package com.youdeyiwu.model.entity.point;

import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Transient;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * point rule.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PointRuleEntity extends AbstractEntity {

  /**
   * rule name.
   */
  @Enumerated(EnumType.STRING)
  @Column(unique = true)
  private RuleNameEnum ruleName;

  /**
   * points awarded to the initiator user.
   */
  private Integer initiatorRewardPoints = 0;

  /**
   * points awarded to the receiver user.
   */
  private Integer receiverRewardPoints = 0;

  /**
   * the user who initiated the point rule.
   */
  @Transient
  private UserEntity initiatorUser;

  /**
   * the user who received the points in the rule.
   */
  @Transient
  private UserEntity receiverUser;

}