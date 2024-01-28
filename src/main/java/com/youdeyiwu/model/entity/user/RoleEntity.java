package com.youdeyiwu.model.entity.user;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
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
 * role.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class RoleEntity extends AbstractEntity {

  /**
   * name.
   */
  @Column(nullable = false)
  private String name;

  /**
   * overview.
   */
  private String overview;

  /**
   * sort.
   */
  @Column(nullable = false)
  private Integer sort = 0;

  /**
   * display.
   */
  @Column(nullable = false)
  private Boolean display = true;

  /**
   * users.
   */
  @ManyToMany(mappedBy = "roles")
  @JsonIgnore
  @ToString.Exclude
  private Set<UserEntity> users = new HashSet<>();

  /**
   * menus.
   */
  @ManyToMany(mappedBy = "roles")
  @JsonIgnore
  @ToString.Exclude
  private Set<MenuEntity> menus = new HashSet<>();

  /**
   * submenus.
   */
  @ManyToMany(mappedBy = "roles")
  @JsonIgnore
  @ToString.Exclude
  private Set<SubmenuEntity> submenus = new HashSet<>();

  /**
   * permissions.
   */
  @ManyToMany(cascade = {
      CascadeType.DETACH,
      CascadeType.MERGE,
      CascadeType.PERSIST,
      CascadeType.REFRESH
  })
  @JsonIgnore
  @ToString.Exclude
  private Set<PermissionEntity> permissions = new HashSet<>();

}