package com.youdeyiwu.service.user.impl;

import com.youdeyiwu.exception.ActionNotFoundException;
import com.youdeyiwu.exception.MenuNotFoundException;
import com.youdeyiwu.exception.RoleNotFoundException;
import com.youdeyiwu.exception.SubmenuNotFoundException;
import com.youdeyiwu.mapper.user.ActionMapper;
import com.youdeyiwu.mapper.user.MenuMapper;
import com.youdeyiwu.mapper.user.RoleMapper;
import com.youdeyiwu.mapper.user.SubmenuMapper;
import com.youdeyiwu.model.dto.user.CreateSubmenuDto;
import com.youdeyiwu.model.dto.user.UpdateRolesSubmenuDto;
import com.youdeyiwu.model.dto.user.UpdateSubmenuDto;
import com.youdeyiwu.model.entity.user.ActionEntity;
import com.youdeyiwu.model.entity.user.MenuEntity;
import com.youdeyiwu.model.entity.user.SubmenuEntity;
import com.youdeyiwu.model.vo.user.SubmenuEntityVo;
import com.youdeyiwu.repository.user.ActionRepository;
import com.youdeyiwu.repository.user.MenuRepository;
import com.youdeyiwu.repository.user.RoleRepository;
import com.youdeyiwu.repository.user.SubmenuRepository;
import com.youdeyiwu.service.user.SubmenuService;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * submenu.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class SubmenuServiceImpl implements SubmenuService {

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
  public SubmenuEntity create(CreateSubmenuDto dto) {
    SubmenuEntity submenuEntity = new SubmenuEntity();
    submenuMapper.dtoToEntity(dto, submenuEntity);
    return submenuRepository.save(submenuEntity);
  }

  @Transactional
  @Override
  public void updateRoles(Long id, UpdateRolesSubmenuDto dto) {
    SubmenuEntity submenuEntity = findSubmenu(id);

    if (Objects.nonNull(dto.roles())) {
      submenuEntity.setRoles(
          dto.roles()
              .stream()
              .map(rid -> roleRepository.findById(rid).orElseThrow(RoleNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void update(Long id, UpdateSubmenuDto dto) {
    SubmenuEntity submenuEntity = findSubmenu(id);
    submenuMapper.dtoToEntity(dto, submenuEntity);

    if (Objects.nonNull(dto.menu())) {
      MenuEntity menuEntity = menuRepository.findById(dto.menu())
          .orElseThrow(MenuNotFoundException::new);
      submenuEntity.setMenu(menuEntity);
    }

    if (Objects.nonNull(dto.actions())) {
      submenuEntity.getActions().clear();
      submenuEntity.getActions().addAll(
          dto.actions()
              .stream()
              .map(aid -> {
                ActionEntity actionEntity = actionRepository.findById(aid)
                    .orElseThrow(ActionNotFoundException::new);
                actionEntity.setSubmenu(submenuEntity);
                return actionEntity;
              })
              .collect(Collectors.toSet())
      );
    }
  }

  @Override
  public SubmenuEntityVo query(Long id) {
    SubmenuEntity submenuEntity = findSubmenu(id);
    SubmenuEntityVo vo = submenuMapper.entityToVo(submenuEntity);
    setMenu(vo, submenuEntity);
    setActions(vo, submenuEntity);
    setRoles(vo, submenuEntity);
    return vo;
  }

  @Override
  public List<SubmenuEntityVo> queryAll() {
    return submenuRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id"))
        .stream()
        .map(submenuEntity -> {
          SubmenuEntityVo vo = submenuMapper.entityToVo(submenuEntity);
          setMenu(vo, submenuEntity);
          setActions(vo, submenuEntity);
          setRoles(vo, submenuEntity);
          return vo;
        })
        .toList();
  }

  @Transactional
  @Override
  public void delete(Long id) {
    SubmenuEntity submenuEntity = findSubmenu(id);
    submenuRepository.delete(submenuEntity);
  }

  /**
   * find submenu.
   *
   * @param id id
   * @return SubmenuEntity
   */
  private SubmenuEntity findSubmenu(Long id) {
    return submenuRepository.findById(id)
        .orElseThrow(SubmenuNotFoundException::new);
  }

  /**
   * set menu.
   *
   * @param vo            vo
   * @param submenuEntity submenuEntity
   */
  private void setMenu(SubmenuEntityVo vo, SubmenuEntity submenuEntity) {
    vo.setMenu(menuMapper.entityToVo(submenuEntity.getMenu()));
  }

  /**
   * set actions.
   *
   * @param vo            vo
   * @param submenuEntity submenuEntity
   */
  private void setActions(SubmenuEntityVo vo, SubmenuEntity submenuEntity) {
    vo.setActions(
        submenuEntity.getActions()
            .stream()
            .map(actionMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set roles.
   *
   * @param vo            vo
   * @param submenuEntity submenuEntity
   */
  private void setRoles(SubmenuEntityVo vo, SubmenuEntity submenuEntity) {
    vo.setRoles(
        submenuEntity.getRoles()
            .stream()
            .map(roleMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }
}