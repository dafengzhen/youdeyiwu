package com.youdeyiwu.service.user;

import com.youdeyiwu.model.dto.user.CreateActionDto;
import com.youdeyiwu.model.dto.user.UpdateActionDto;
import com.youdeyiwu.model.entity.user.ActionEntity;
import com.youdeyiwu.model.vo.user.ActionEntityVo;
import java.util.Set;

/**
 * action.
 *
 * @author dafengzhen
 */
public interface ActionService {

  /**
   * create.
   *
   * @param dto dto
   * @return ActionEntity
   */
  ActionEntity create(CreateActionDto dto);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdateActionDto dto);

  /**
   * query.
   *
   * @param id id
   * @return ActionEntityVo
   */
  ActionEntityVo query(Long id);

  /**
   * query all.
   *
   * @return Set
   */
  Set<ActionEntityVo> queryAll();

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}