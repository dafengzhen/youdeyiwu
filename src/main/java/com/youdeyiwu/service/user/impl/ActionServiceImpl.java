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
import com.youdeyiwu.model.dto.user.UpdateRoleActionDto;
import com.youdeyiwu.model.entity.user.ActionEntity;
import com.youdeyiwu.model.vo.user.ActionEntityVo;
import com.youdeyiwu.repository.user.ActionRepository;
import com.youdeyiwu.repository.user.MenuRepository;
import com.youdeyiwu.repository.user.RoleRepository;
import com.youdeyiwu.repository.user.SubmenuRepository;
import com.youdeyiwu.service.user.ActionService;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

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

    if (!StringUtils.hasText(dto.alias())) {
      actionEntity.setAlias(dto.name());
    }

    actionRepository.save(actionEntity);
    return actionEntity;
  }

  @Transactional
  @Override
  public void updateRole(Long id, UpdateRoleActionDto dto) {
    ActionEntity actionEntity = findAction(id);

    actionEntity.setRole(
        roleRepository.findById(dto.role()).orElseThrow(RoleNotFoundException::new)
    );
  }

  @Transactional
  @Override
  public void update(Long id, UpdateActionDto dto) {
    ActionEntity actionEntity = findAction(id);
    actionMapper.dtoToEntity(dto, actionEntity);

    if (!StringUtils.hasText(dto.alias())) {
      actionEntity.setAlias(dto.name());
    }

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
    setRole(vo, actionEntity);
    return vo;
  }

  @Override
  public Set<ActionEntityVo> queryAll() {
    return StreamSupport.stream(
            actionRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id"))
                .spliterator(),
            false
        )
        .map(actionEntity -> {
          ActionEntityVo vo = actionMapper.entityToVo(actionEntity);
          setMenu(vo, actionEntity);
          setSubmenu(vo, actionEntity);
          setRole(vo, actionEntity);
          return vo;
        })
        .collect(Collectors.toSet());
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
   * set role.
   *
   * @param vo           vo
   * @param actionEntity actionEntity
   */
  private void setRole(ActionEntityVo vo, ActionEntity actionEntity) {
    vo.setRole(roleMapper.entityToVo(actionEntity.getRole()));
  }
}