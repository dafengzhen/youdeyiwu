package com.youdeyiwu.model.entity.user;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import java.util.HashSet;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * action.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class ActionEntity extends AbstractEntity {

  /**
   * name.
   * example: page#action > section#create
   */
  @Column(nullable = false)
  private String name;

  /**
   * alias.
   */
  private String alias;

  /**
   * sort.
   */
  @Column(nullable = false)
  private Integer sort = 0;

  /**
   * menu.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private MenuEntity menu;

  /**
   * submenu.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private SubmenuEntity submenu;

  /**
   * roles.
   */
  @ManyToMany(cascade = {
      CascadeType.DETACH,
      CascadeType.MERGE,
      CascadeType.PERSIST,
      CascadeType.REFRESH
  })
  @JsonIgnore
  @ToString.Exclude
  private Set<RoleEntity> roles = new HashSet<>();

}