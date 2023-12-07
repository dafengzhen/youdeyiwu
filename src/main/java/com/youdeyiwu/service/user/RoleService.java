package com.youdeyiwu.service.user;

import com.youdeyiwu.model.dto.user.AssignPermissionsDto;
import com.youdeyiwu.model.dto.user.CreateRoleDto;
import com.youdeyiwu.model.dto.user.UpdatePermissionsRoleDto;
import com.youdeyiwu.model.dto.user.UpdateRoleDto;
import com.youdeyiwu.model.entity.user.RoleEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.user.RoleEntityVo;
import com.youdeyiwu.model.vo.user.RolePermissionsVo;
import org.springframework.data.domain.Pageable;

/**
 * role.
 *
 * @author dafengzhen
 */
public interface RoleService {

  /**
   * create.
   *
   * @param dto dto
   * @return RoleEntity
   */
  RoleEntity create(CreateRoleDto dto);

  /**
   * addPermissions.
   *
   * @param id  id
   * @param dto dto
   */
  void addPermissions(Long id, AssignPermissionsDto dto);

  /**
   * removePermissions.
   *
   * @param id  id
   * @param dto dto
   */
  void removePermissions(Long id, AssignPermissionsDto dto);

  /**
   * update permissions (The difference with this method is that it updates the entire permissions list).
   *
   * @param id  id
   * @param dto dto
   */
  void updatePermissions(Long id, UpdatePermissionsRoleDto dto);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdateRoleDto dto);

  /**
   * getPermissions.
   *
   * @param id id
   * @return RolePermissionsVo
   */
  RolePermissionsVo getPermissions(Long id);

  /**
   * query.
   *
   * @param id id
   * @return RoleEntityVo
   */
  RoleEntityVo query(Long id);

  /**
   * query all.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<RoleEntityVo> queryAll(Pageable pageable);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}