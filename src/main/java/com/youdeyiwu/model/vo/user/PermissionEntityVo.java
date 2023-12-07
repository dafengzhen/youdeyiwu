package com.youdeyiwu.model.vo.user;

import com.youdeyiwu.enums.permission.MatcherTypeEnum;
import com.youdeyiwu.enums.permission.MethodTypeEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * permission.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PermissionEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
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
  private MethodTypeEnum method;

  /**
   * type.
   */
  private MatcherTypeEnum type;

  /**
   * case sensitive.
   */
  private Boolean caseInsensitive;

  /**
   * sort.
   */
  private Integer sort;

  /**
   * matcher.
   */
  private PermissionEntityVo matcher;

  /**
   * matchers.
   */
  private Set<PermissionEntityVo> matchers;

  /**
   * role.
   */
  private RoleEntityVo role;

  /**
   * roles.
   */
  private Set<RoleEntityVo> roles;

}