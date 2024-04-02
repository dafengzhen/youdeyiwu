package com.youdeyiwu.model.entity.config;

import com.youdeyiwu.constant.JwtConfigConstant;
import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.constant.PostConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import lombok.Setter;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * config.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Table(uniqueConstraints = {@UniqueConstraint(columnNames = {"type", "name"})})
@Entity
public class ConfigEntity extends AbstractEntity {

  /**
   * prefix.
   */
  @Enumerated(EnumType.STRING)
  @Column(nullable = false)
  private ConfigTypeEnum type;

  /**
   * name.
   * {@link JwtConfigConstant}
   * {@link PointConfigConstant}
   * {@link PostConfigConstant}
   */
  @Column(nullable = false)
  private String name;

  /**
   * value.
   */
  @Column(columnDefinition = "text", nullable = false)
  private String value;

  /**
   * description.
   */
  private String description;

}