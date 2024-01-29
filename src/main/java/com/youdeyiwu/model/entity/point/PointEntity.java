package com.youdeyiwu.model.entity.point;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Transient;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * point.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PointEntity extends AbstractEntity {

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
   * user.
   */
  @OneToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity user;

  /**
   * old points.
   */
  @Transient
  private Integer oldPoints;

}