package com.youdeyiwu.service.user.impl;

import com.youdeyiwu.exception.ActionNotFoundException;
import com.youdeyiwu.exception.MenuNotFoundException;
import com.youdeyiwu.exception.RoleNotFoundException;
import com.youdeyiwu.exception.SubmenuNotFoundException;
import com.youdeyiwu.mapper.user.ActionMapper;
import com.youdeyiwu.mapper.user.MenuMapper;
import com.youdeyiwu.mapper.user.RoleMapper;
import com.youdeyiwu.mapper.user.SubmenuMapper;
import com.youdeyiwu.model.dto.user.CreateActionDto;
import com.youdeyiwu.model.dto.user.UpdateActionDto;
import com.youdeyiwu.model.dto.user.UpdateRolesActionDto;
import com.youdeyiwu.model.entity.user.ActionEntity;
import com.youdeyiwu.model.vo.user.ActionEntityVo;
import com.youdeyiwu.repository.user.ActionRepository;
import com.youdeyiwu.repository.user.MenuRepository;
import com.youdeyiwu.repository.user.RoleRepository;
import com.youdeyiwu.repository.user.SubmenuRepository;
import com.youdeyiwu.service.user.ActionService;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * action.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class ActionServiceImpl implements ActionService {

  private final MenuRepository menuRepository;

  private final SubmenuRepository submenuRepository;

  private final ActionRepository actionRepository;

  private final RoleRepository roleRepository;

  private final MenuMapper menuMapper;

  private final SubmenuMapper submenuMapper;

  private final ActionMapper actionMapper;

  private final RoleMapper roleMapper;

  @Transactional
  @Override
  public ActionEntity create(CreateActionDto dto) {
    ActionEntity actionEntity = new ActionEntity();
    actionMapper.dtoToEntity(dto, actionEntity);
    actionRepository.save(actionEntity);
    return actionEntity;
  }

  @Transactional
  @Override
  public void updateRoles(Long id, UpdateRolesActionDto dto) {
    ActionEntity actionEntity = findAction(id);

    if (Objects.nonNull(dto.roles())) {
      actionEntity.setRoles(
          dto.roles()
              .stream()
              .map(rid -> roleRepository.findById(rid).orElseThrow(RoleNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void update(Long id, UpdateActionDto dto) {
    ActionEntity actionEntity = findAction(id);
    actionMapper.dtoToEntity(dto, actionEntity);

    if (Objects.nonNull(dto.menu())) {
      actionEntity.setMenu(
          menuRepository.findById(dto.menu())
              .orElseThrow(MenuNotFoundException::new)
      );
    }

    if (Objects.nonNull(dto.submenu())) {
      actionEntity.setSubmenu(
          submenuRepository.findById(dto.submenu())
              .orElseThrow(SubmenuNotFoundException::new)
      );
    }
  }

  @Override
  public ActionEntityVo query(Long id) {
    ActionEntity actionEntity = findAction(id);
    ActionEntityVo vo = actionMapper.entityToVo(actionEntity);
    setMenu(vo, actionEntity);
    setSubmenu(vo, actionEntity);
    setRoles(vo, actionEntity);
    return vo;
  }

  @Override
  public List<ActionEntityVo> queryAll() {
    return actionRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id"))
        .stream()
        .map(actionEntity -> {
          ActionEntityVo vo = actionMapper.entityToVo(actionEntity);
          setMenu(vo, actionEntity);
          setSubmenu(vo, actionEntity);
          setRoles(vo, actionEntity);
          return vo;
        })
        .toList();
  }

  @Transactional
  @Override
  public void delete(Long id) {
    ActionEntity actionEntity = findAction(id);
    actionRepository.delete(actionEntity);
  }

  /**
   * find action.
   *
   * @param id id
   * @return ActionEntity
   */
  private ActionEntity findAction(Long id) {
    return actionRepository.findById(id)
        .orElseThrow(ActionNotFoundException::new);
  }

  /**
   * set menu.
   *
   * @param vo           vo
   * @param actionEntity actionEntity
   */
  private void setMenu(ActionEntityVo vo, ActionEntity actionEntity) {
    vo.setMenu(menuMapper.entityToVo(actionEntity.getMenu()));
  }

  /**
   * set submenu.
   *
   * @param vo           vo
   * @param actionEntity actionEntity
   */
  private void setSubmenu(ActionEntityVo vo, ActionEntity actionEntity) {
    vo.setSubmenu(submenuMapper.entityToVo(actionEntity.getSubmenu()));
  }

  /**
   * set roles.
   *
   * @param vo           vo
   * @param actionEntity actionEntity
   */
  private void setRoles(ActionEntityVo vo, ActionEntity actionEntity) {
    vo.setRoles(
        actionEntity.getRoles()
            .stream()
            .map(roleMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }
}