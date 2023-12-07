package com.youdeyiwu.model.entity.user;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.permission.MatcherTypeEnum;
import com.youdeyiwu.enums.permission.MethodTypeEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Enumerated;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import java.util.HashSet;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * permission.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PermissionEntity extends AbstractEntity {

  /**
   * name.
   */
  @Column(nullable = false)
  private String name;

  /**
   * alias.
   */
  private String alias;

  /**
   * overview.
   */
  private String overview;

  /**
   * method.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private MethodTypeEnum method = MethodTypeEnum.GET;

  /**
   * type.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private MatcherTypeEnum type = MatcherTypeEnum.ANT;

  /**
   * case sensitive.
   */
  @Column(nullable = false)
  private Boolean caseInsensitive = false;

  /**
   * sort.
   */
  @Column(nullable = false)
  private Integer sort = 0;

  /**
   * matchers.
   */
  @OneToMany(
      mappedBy = "matcher",
      cascade = CascadeType.ALL
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<PermissionEntity> matchers = new HashSet<>();

  /**
   * matcher.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private PermissionEntity matcher;

  /**
   * roles.
   */
  @ManyToMany(mappedBy = "permissions")
  @JsonIgnore
  @ToString.Exclude
  private Set<RoleEntity> roles = new HashSet<>();

}