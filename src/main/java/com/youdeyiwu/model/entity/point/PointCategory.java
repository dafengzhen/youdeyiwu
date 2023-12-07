package com.youdeyiwu.model.entity.point;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.ActionEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.ManyToMany;
import java.util.HashSet;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * point category.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PointCategory extends AbstractEntity {

  /**
   * name.
   */
  private String name;

  /**
   * weight.
   */
  private int weight = 1;

  /**
   * actions.
   */
  @ManyToMany(cascade = {
      CascadeType.DETACH,
      CascadeType.MERGE,
      CascadeType.PERSIST,
      CascadeType.REFRESH
  })
  @JsonIgnore
  @ToString.Exclude
  private Set<ActionEntity> actions = new HashSet<>();

}
