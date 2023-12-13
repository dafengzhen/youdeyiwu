package com.youdeyiwu.service.user.impl;

import com.youdeyiwu.exception.MenuNotFoundException;
import com.youdeyiwu.exception.SubmenuNotFoundException;
import com.youdeyiwu.mapper.user.MenuMapper;
import com.youdeyiwu.mapper.user.SubmenuMapper;
import com.youdeyiwu.model.dto.user.CreateMenuDto;
import com.youdeyiwu.model.dto.user.UpdateMenuDto;
import com.youdeyiwu.model.entity.user.MenuEntity;
import com.youdeyiwu.model.vo.user.MenuEntityVo;
import com.youdeyiwu.repository.user.MenuRepository;
import com.youdeyiwu.repository.user.SubmenuRepository;
import com.youdeyiwu.service.user.MenuService;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
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

  private final MenuMapper menuMapper;

  private final SubmenuMapper submenuMapper;

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

    if (Objects.nonNull(dto.submenus())) {
      menuEntity.setSubmenus(
          dto.submenus()
              .stream()
              .map(sid -> submenuRepository.findById(sid)
                  .orElseThrow(SubmenuNotFoundException::new)
              )
              .collect(Collectors.toSet())
      );
    }
  }

  @Override
  public MenuEntityVo query(Long id) {
    MenuEntity menuEntity = menuRepository.findById(id)
        .orElseThrow(MenuNotFoundException::new);
    MenuEntityVo vo = menuMapper.entityToVo(menuEntity);
    setSubmenus(vo, menuEntity);
    return vo;
  }

  @Override
  public Set<MenuEntityVo> queryAll() {
    return StreamSupport.stream(
            menuRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id"))
                .spliterator(),
            false
        )
        .map(menuEntity -> {
          MenuEntityVo vo = menuMapper.entityToVo(menuEntity);
          setSubmenus(vo, menuEntity);
          return vo;
        })
        .collect(Collectors.toSet());
  }

  @Transactional
  @Override
  public void delete(Long id) {
    MenuEntity menuEntity = menuRepository.findById(id)
        .orElseThrow(MenuNotFoundException::new);
    menuRepository.delete(menuEntity);
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
}