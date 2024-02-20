package com.youdeyiwu.service.user;

import com.youdeyiwu.model.dto.user.CreateMenuDto;
import com.youdeyiwu.model.dto.user.UpdateMenuDto;
import com.youdeyiwu.model.dto.user.UpdateRolesMenuDto;
import com.youdeyiwu.model.entity.user.MenuEntity;
import com.youdeyiwu.model.vo.user.MenuEntityVo;
import java.util.List;

/**
 * menu.
 *
 * @author dafengzhen
 */
public interface MenuService {

  /**
   * create.
   *
   * @param dto dto
   * @return MenuEntity
   */
  MenuEntity create(CreateMenuDto dto);

  /**
   * update roles menu.
   *
   * @param id  id
   * @param dto dto
   */
  void updateRoles(Long id, UpdateRolesMenuDto dto);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdateMenuDto dto);

  /**
   * query.
   *
   * @param id id
   * @return MenuEntityVo
   */
  MenuEntityVo query(Long id);

  /**
   * query all.
   *
   * @return List
   */
  List<MenuEntityVo> queryAll();

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}