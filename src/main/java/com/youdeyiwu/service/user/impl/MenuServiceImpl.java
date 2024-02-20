package com.youdeyiwu.service.user.impl;

import com.youdeyiwu.exception.ActionNotFoundException;
import com.youdeyiwu.exception.MenuNotFoundException;
import com.youdeyiwu.exception.RoleNotFoundException;
import com.youdeyiwu.exception.SubmenuNotFoundException;
import com.youdeyiwu.mapper.user.ActionMapper;
import com.youdeyiwu.mapper.user.MenuMapper;
import com.youdeyiwu.mapper.user.RoleMapper;
import com.youdeyiwu.mapper.user.SubmenuMapper;
import com.youdeyiwu.model.dto.user.CreateMenuDto;
import com.youdeyiwu.model.dto.user.UpdateMenuDto;
import com.youdeyiwu.model.dto.user.UpdateRolesMenuDto;
import com.youdeyiwu.model.entity.user.ActionEntity;
import com.youdeyiwu.model.entity.user.MenuEntity;
import com.youdeyiwu.model.entity.user.SubmenuEntity;
import com.youdeyiwu.model.vo.user.MenuEntityVo;
import com.youdeyiwu.repository.user.ActionRepository;
import com.youdeyiwu.repository.user.MenuRepository;
import com.youdeyiwu.repository.user.RoleRepository;
import com.youdeyiwu.repository.user.SubmenuRepository;
import com.youdeyiwu.service.user.MenuService;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * menu.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class MenuServiceImpl implements MenuService {

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
  public MenuEntity create(CreateMenuDto dto) {
    MenuEntity menuEntity = new MenuEntity();
    menuMapper.dtoToEntity(dto, menuEntity);
    menuRepository.save(menuEntity);
    return menuEntity;
  }

  @Transactional
  @Override
  public void updateRoles(Long id, UpdateRolesMenuDto dto) {
    MenuEntity menuEntity = findMenu(id);

    if (Objects.nonNull(dto.roles())) {
      menuEntity.setRoles(
          dto.roles()
              .stream()
              .map(rid -> roleRepository.findById(rid).orElseThrow(RoleNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void update(Long id, UpdateMenuDto dto) {
    MenuEntity menuEntity = findMenu(id);
    menuMapper.dtoToEntity(dto, menuEntity);

    if (Objects.nonNull(dto.submenus())) {
      menuEntity.getSubmenus().clear();
      menuEntity.getSubmenus().addAll(
          dto.submenus()
              .stream()
              .map(sid -> {
                SubmenuEntity submenuEntity = submenuRepository.findById(sid)
                    .orElseThrow(SubmenuNotFoundException::new);
                submenuEntity.setMenu(menuEntity);
                return submenuEntity;
              })
              .collect(Collectors.toSet())
      );
    }

    if (Objects.nonNull(dto.actions())) {
      menuEntity.getActions().clear();
      menuEntity.getActions().addAll(
          dto.actions()
              .stream()
              .map(aid -> {
                ActionEntity actionEntity = actionRepository.findById(aid)
                    .orElseThrow(ActionNotFoundException::new);
                actionEntity.setMenu(menuEntity);
                return actionEntity;
              })
              .collect(Collectors.toSet())
      );
    }
  }

  @Override
  public MenuEntityVo query(Long id) {
    MenuEntity menuEntity = findMenu(id);
    MenuEntityVo vo = menuMapper.entityToVo(menuEntity);
    setSubmenus(vo, menuEntity);
    setActions(vo, menuEntity);
    setRoles(vo, menuEntity);
    return vo;
  }

  @Override
  public List<MenuEntityVo> queryAll() {
    return menuRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id"))
        .stream()
        .map(menuEntity -> {
          MenuEntityVo vo = menuMapper.entityToVo(menuEntity);
          setSubmenus(vo, menuEntity);
          setActions(vo, menuEntity);
          setRoles(vo, menuEntity);
          return vo;
        })
        .toList();
  }

  @Transactional
  @Override
  public void delete(Long id) {
    MenuEntity menuEntity = findMenu(id);
    menuRepository.delete(menuEntity);
  }

  /**
   * find menu.
   *
   * @param id id
   * @return MenuEntity
   */
  private MenuEntity findMenu(Long id) {
    return menuRepository.findById(id)
        .orElseThrow(MenuNotFoundException::new);
  }

  /**
   * set submenus.
   *
   * @param vo         vo
   * @param menuEntity menuEntity
   */
  private void setSubmenus(MenuEntityVo vo, MenuEntity menuEntity) {
    vo.setSubmenus(
        menuEntity.getSubmenus().stream()
            .map(submenuMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set actions.
   *
   * @param vo         vo
   * @param menuEntity menuEntity
   */
  private void setActions(MenuEntityVo vo, MenuEntity menuEntity) {
    vo.setActions(
        menuEntity.getActions().stream()
            .map(actionMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set roles.
   *
   * @param vo         vo
   * @param menuEntity menuEntity
   */
  private void setRoles(MenuEntityVo vo, MenuEntity menuEntity) {
    vo.setRoles(
        menuEntity.getRoles().stream()
            .map(roleMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }
}