package com.youdeyiwu.service.forum.impl;

import com.youdeyiwu.exception.SectionNotFoundException;
import com.youdeyiwu.exception.TagGroupNotFoundException;
import com.youdeyiwu.mapper.forum.SectionGroupMapper;
import com.youdeyiwu.mapper.forum.SectionMapper;
import com.youdeyiwu.mapper.user.UserMapper;
import com.youdeyiwu.model.dto.forum.CreateSectionGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionsSectionGroupDto;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.forum.SectionGroupEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.SectionEntityVo;
import com.youdeyiwu.model.vo.forum.SectionGroupEntityVo;
import com.youdeyiwu.repository.forum.SectionGroupRepository;
import com.youdeyiwu.repository.forum.SectionRepository;
import com.youdeyiwu.service.forum.SectionGroupService;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * section group.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class SectionGroupServiceImpl implements SectionGroupService {

  private final SectionGroupRepository sectionGroupRepository;

  private final SectionRepository sectionRepository;

  private final SectionGroupMapper sectionGroupMapper;

  private final SectionMapper sectionMapper;

  private final UserMapper userMapper;

  @Transactional
  @Override
  public SectionGroupEntity create(CreateSectionGroupDto dto) {
    SectionGroupEntity sectionGroupEntity = new SectionGroupEntity();
    sectionGroupMapper.dtoToEntity(dto, sectionGroupEntity);
    sectionGroupRepository.save(sectionGroupEntity);
    return sectionGroupEntity;
  }

  @Transactional
  @Override
  public void updateSections(Long id, UpdateSectionsSectionGroupDto dto) {
    SectionGroupEntity sectionGroupEntity = sectionGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);

    if (Objects.nonNull(dto.sections())) {
      sectionGroupEntity.setSections(
          dto.sections()
              .stream()
              .map(sid -> sectionRepository.findById(sid)
                  .orElseThrow(SectionNotFoundException::new)
              )
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void update(Long id, UpdateSectionGroupDto dto) {
    SectionGroupEntity sectionGroupEntity = sectionGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);

    if (StringUtils.hasText(dto.name())) {
      sectionGroupEntity.setName(dto.name());
    }

    if (Objects.nonNull(dto.sort())) {
      sectionGroupEntity.setSort(dto.sort());
    }
  }

  @Override
  public PageVo<SectionGroupEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(
        sectionGroupRepository.findAll(pageable)
            .map(sectionGroupEntity -> {
              SectionGroupEntityVo vo = sectionGroupMapper.entityToVo(sectionGroupEntity);
              setSections(vo, sectionGroupEntity);
              return vo;
            })
    );
  }

  @Override
  public Set<SectionGroupEntityVo> selectAll() {
    return StreamSupport.stream(sectionGroupRepository.findAll().spliterator(), false)
        .map(sectionGroupEntity -> {
          SectionGroupEntityVo vo = sectionGroupMapper.entityToVo(sectionGroupEntity);
          setSections(vo, sectionGroupEntity);
          return vo;
        })
        .collect(Collectors.toSet());
  }

  @Override
  public SectionGroupEntityVo query(Long id) {
    SectionGroupEntity sectionGroupEntity = sectionGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);

    SectionGroupEntityVo vo = sectionGroupMapper.entityToVo(sectionGroupEntity);
    setSections(vo, sectionGroupEntity);
    return vo;
  }

  @Transactional
  @Override
  public void delete(Long id) {
    SectionGroupEntity sectionGroupEntity = sectionGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);
    sectionGroupRepository.delete(sectionGroupEntity);
  }

  /**
   * set sections.
   *
   * @param vo                 vo
   * @param sectionGroupEntity sectionGroupEntity
   */
  private void setSections(SectionGroupEntityVo vo, SectionGroupEntity sectionGroupEntity) {
    vo.setSections(
        sectionGroupEntity.getSections()
            .stream()
            .map(sectionEntity -> {
              SectionEntityVo sectionEntityVo = sectionMapper.entityToVo(sectionEntity);
              setAdmins(sectionEntityVo, sectionEntity);
              return sectionEntityVo;
            })
            .collect(Collectors.toSet())
    );
  }

  /**
   * set admins.
   *
   * @param vo            vo
   * @param sectionEntity sectionEntity
   */
  private void setAdmins(SectionEntityVo vo, SectionEntity sectionEntity) {
    vo.setAdmins(
        sectionEntity.getAdmins()
            .stream()
            .map(userMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }
}