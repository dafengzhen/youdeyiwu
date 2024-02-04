package com.youdeyiwu.model.entity.point;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
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
 * point permission rule.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PointPermissionRuleEntity extends AbstractEntity {

  /**
   * permission rule name.
   */
  @Enumerated(EnumType.STRING)
  @Column(unique = true)
  private PermissionRuleNameEnum permissionRuleName;

  /**
   * required points.
   */
  private Integer requiredPoints = 0;

}