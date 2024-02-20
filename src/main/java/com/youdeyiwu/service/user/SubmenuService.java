package com.youdeyiwu.service.user;

import com.youdeyiwu.model.dto.user.CreateSubmenuDto;
import com.youdeyiwu.model.dto.user.UpdateRolesSubmenuDto;
import com.youdeyiwu.model.dto.user.UpdateSubmenuDto;
import com.youdeyiwu.model.entity.user.SubmenuEntity;
import com.youdeyiwu.model.vo.user.SubmenuEntityVo;
import java.util.List;

/**
 * submenu.
 *
 * @author dafengzhen
 */
public interface SubmenuService {

  /**
   * create.
   *
   * @param dto dto
   * @return SubmenuEntity
   */
  SubmenuEntity create(CreateSubmenuDto dto);

  /**
   * update roles menu.
   *
   * @param id  id
   * @param dto dto
   */
  void updateRoles(Long id, UpdateRolesSubmenuDto dto);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdateSubmenuDto dto);

  /**
   * query.
   *
   * @param id id
   * @return SubmenuEntityVo
   */
  SubmenuEntityVo query(Long id);

  /**
   * query all.
   *
   * @return List
   */
  List<SubmenuEntityVo> queryAll();

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}