package com.youdeyiwu.service.user.impl;

import com.youdeyiwu.exception.MenuNotFoundException;
import com.youdeyiwu.exception.RoleNotFoundException;
import com.youdeyiwu.mapper.user.MenuMapper;
import com.youdeyiwu.mapper.user.PermissionMapper;
import com.youdeyiwu.mapper.user.RoleMapper;
import com.youdeyiwu.model.dto.user.CreateMenuDto;
import com.youdeyiwu.model.dto.user.UpdateMenuDto;
import com.youdeyiwu.model.entity.user.MenuEntity;
import com.youdeyiwu.model.entity.user.RoleEntity;
import com.youdeyiwu.model.vo.user.MenuEntityVo;
import com.youdeyiwu.model.vo.user.RoleEntityVo;
import com.youdeyiwu.repository.user.MenuRepository;
import com.youdeyiwu.repository.user.PermissionRepository;
import com.youdeyiwu.repository.user.RoleRepository;
import com.youdeyiwu.service.user.MenuService;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
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

  private final RoleMapper roleMapper;

  private final RoleRepository roleRepository;

  private final PermissionRepository permissionRepository;

  private final MenuRepository menuRepository;

  private final PermissionMapper permissionMapper;

  private final MenuMapper menuMapper;

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
  public void update(Long id, UpdateMenuDto dto) {
    MenuEntity menuEntity = menuRepository.findById(id)
        .orElseThrow(MenuNotFoundException::new);
    menuMapper.dtoToEntity(dto, menuEntity);
  }

  @Override
  public MenuEntityVo query(Long id) {
    MenuEntity menuEntity = menuRepository.findById(id)
        .orElseThrow(MenuNotFoundException::new);

    MenuEntityVo vo = menuMapper.entityToVo(menuEntity);
    return vo;
  }

  @Override
  public List<MenuEntityVo> queryAll() {
//    return menuRepository.findAll().map(menuEntity -> {
//      MenuEntityVo vo = menuMapper.entityToVo(menuEntity);
//      return vo;
//    })
    return new ArrayList<>();
  }

  @Transactional
  @Override
  public void delete(Long id) {
    RoleEntity roleEntity = roleRepository.findById(id)
        .orElseThrow(RoleNotFoundException::new);
    roleRepository.delete(roleEntity);
  }

  /**
   * set permissions.
   *
   * @param vo         vo
   * @param roleEntity roleEntity
   */
  private void setPermissions(RoleEntityVo vo, RoleEntity roleEntity) {
    vo.setPermissions(
        roleEntity.getPermissions()
            .stream()
            .map(permissionMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }
}