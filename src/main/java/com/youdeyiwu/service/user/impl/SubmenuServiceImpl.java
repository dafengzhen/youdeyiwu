package com.youdeyiwu.service.user.impl;

import com.youdeyiwu.exception.MenuNotFoundException;
import com.youdeyiwu.exception.SubmenuNotFoundException;
import com.youdeyiwu.mapper.user.MenuMapper;
import com.youdeyiwu.mapper.user.SubmenuMapper;
import com.youdeyiwu.model.dto.user.CreateSubmenuDto;
import com.youdeyiwu.model.dto.user.UpdateSubmenuDto;
import com.youdeyiwu.model.entity.user.MenuEntity;
import com.youdeyiwu.model.entity.user.SubmenuEntity;
import com.youdeyiwu.model.vo.user.SubmenuEntityVo;
import com.youdeyiwu.repository.user.MenuRepository;
import com.youdeyiwu.repository.user.SubmenuRepository;
import com.youdeyiwu.service.user.SubmenuService;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
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

  private final SubmenuMapper submenuMapper;

  private final MenuMapper menuMapper;

  @Transactional
  @Override
  public SubmenuEntity create(CreateSubmenuDto dto) {
    SubmenuEntity submenuEntity = new SubmenuEntity();
    submenuMapper.dtoToEntity(dto, submenuEntity);
    submenuRepository.save(submenuEntity);
    return submenuEntity;
  }

  @Transactional
  @Override
  public void update(Long id, UpdateSubmenuDto dto) {
    SubmenuEntity submenuEntity = submenuRepository.findById(id)
        .orElseThrow(SubmenuNotFoundException::new);
    submenuMapper.dtoToEntity(dto, submenuEntity);

    if (Objects.nonNull(dto.menu())) {
      MenuEntity menuEntity = menuRepository.findById(dto.menu())
          .orElseThrow(MenuNotFoundException::new);
      submenuEntity.setMenu(menuEntity);
    }
  }

  @Override
  public SubmenuEntityVo query(Long id) {
    SubmenuEntity submenuEntity = submenuRepository.findById(id)
        .orElseThrow(SubmenuNotFoundException::new);
    SubmenuEntityVo vo = submenuMapper.entityToVo(submenuEntity);
    setMenu(vo, submenuEntity);
    return vo;
  }

  @Override
  public Set<SubmenuEntityVo> queryAll() {
    return StreamSupport.stream(
            submenuRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id"))
                .spliterator(),
            false
        )
        .map(submenuEntity -> {
          SubmenuEntityVo vo = submenuMapper.entityToVo(submenuEntity);
          setMenu(vo, submenuEntity);
          return vo;
        })
        .collect(Collectors.toSet());
  }

  @Transactional
  @Override
  public void delete(Long id) {
    SubmenuEntity submenuEntity = submenuRepository.findById(id)
        .orElseThrow(SubmenuNotFoundException::new);
    submenuRepository.delete(submenuEntity);
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
}