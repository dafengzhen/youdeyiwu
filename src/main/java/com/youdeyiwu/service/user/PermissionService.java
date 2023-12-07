package com.youdeyiwu.service.user;

import com.youdeyiwu.model.dto.user.CreatePermissionDto;
import com.youdeyiwu.model.dto.user.UpdatePermissionDto;
import com.youdeyiwu.model.dto.user.UpdateRolesPermissionDto;
import com.youdeyiwu.model.entity.user.PermissionEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.user.PermissionEntityVo;
import org.springframework.data.domain.Pageable;

/**
 * permission.
 *
 * @author dafengzhen
 */
public interface PermissionService {

  /**
   * 创建.
   *
   * @param dto dto
   * @return PermissionEntity
   */
  PermissionEntity create(CreatePermissionDto dto);

  /**
   * update roles.
   *
   * @param id  id
   * @param dto dto
   */
  void updateRoles(Long id, UpdateRolesPermissionDto dto);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdatePermissionDto dto);

  /**
   * query.
   *
   * @param id id
   * @return PermissionEntityVo
   */
  PermissionEntityVo query(Long id);

  /**
   * query all.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<PermissionEntityVo> queryAll(Pageable pageable);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}