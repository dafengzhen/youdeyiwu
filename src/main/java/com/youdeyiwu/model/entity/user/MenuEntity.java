package com.youdeyiwu.model.entity.user;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.OneToMany;
import java.util.HashSet;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * menu.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class MenuEntity extends AbstractEntity {

  /**
   * name.
   */
  @Column(nullable = false)
  private String name;

  /**
   * link.
   */
  @Column(nullable = false)
  private String link;

  /**
   * sort.
   */
  @Column(nullable = false)
  private Integer sort = 0;

  /**
   * submenus.
   */
  @OneToMany(
      mappedBy = "menu",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<SubmenuEntity> submenus = new HashSet<>();

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

  /**
   * actions.
   */
  @OneToMany(
      mappedBy = "menu",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<ActionEntity> actions = new HashSet<>();

}