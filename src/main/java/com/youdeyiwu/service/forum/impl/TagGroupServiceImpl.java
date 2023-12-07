package com.youdeyiwu.service.forum.impl;

import com.youdeyiwu.exception.TagGroupNotFoundException;
import com.youdeyiwu.exception.TagNotFoundException;
import com.youdeyiwu.mapper.forum.TagGroupMapper;
import com.youdeyiwu.mapper.forum.TagMapper;
import com.youdeyiwu.model.dto.forum.CreateTagGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateTagGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsTagGroupDto;
import com.youdeyiwu.model.entity.forum.TagGroupEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.TagGroupEntityVo;
import com.youdeyiwu.repository.forum.TagGroupRepository;
import com.youdeyiwu.repository.forum.TagRepository;
import com.youdeyiwu.service.forum.TagGroupService;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * tag group.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class TagGroupServiceImpl implements TagGroupService {

  private final TagGroupRepository tagGroupRepository;

  private final TagRepository tagRepository;

  private final TagGroupMapper tagGroupMapper;

  private final TagMapper tagMapper;

  @Transactional
  @Override
  public TagGroupEntity create(CreateTagGroupDto dto) {
    TagGroupEntity tagGroupEntity = new TagGroupEntity();
    tagGroupMapper.dtoToEntity(dto, tagGroupEntity);
    tagGroupRepository.save(tagGroupEntity);
    return tagGroupEntity;
  }

  @Transactional
  @Override
  public void updateTags(Long id, UpdateTagsTagGroupDto dto) {
    TagGroupEntity tagGroupEntity = tagGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);

    if (Objects.nonNull(dto.tags())) {
      tagGroupEntity.setTags(
          dto.tags()
              .stream()
              .map(tid -> tagRepository.findById(tid).orElseThrow(TagNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void update(Long id, UpdateTagGroupDto dto) {
    TagGroupEntity tagGroupEntity = tagGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);

    if (StringUtils.hasText(dto.name())) {
      tagGroupEntity.setName(dto.name());
    }

    if (Objects.nonNull(dto.sort())) {
      tagGroupEntity.setSort(dto.sort());
    }
  }

  @Override
  public PageVo<TagGroupEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(tagGroupRepository.findAll(pageable).map(tagGroupEntity -> {
      TagGroupEntityVo vo = tagGroupMapper.entityToVo(tagGroupEntity);
      vo.setTags(
          tagGroupEntity.getTags()
              .stream()
              .map(tagMapper::entityToVo)
              .collect(Collectors.toSet())
      );
      return vo;
    }));
  }

  @Override
  public TagGroupEntityVo query(Long id) {
    TagGroupEntity tagGroupEntity = tagGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);

    TagGroupEntityVo vo = tagGroupMapper.entityToVo(tagGroupEntity);
    vo.setTags(
        tagGroupEntity.getTags()
            .stream()
            .map(tagMapper::entityToVo)
            .collect(Collectors.toSet())
    );
    return vo;
  }

  @Transactional
  @Override
  public void delete(Long id) {
    TagGroupEntity tagGroupEntity = tagGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);
    tagGroupRepository.delete(tagGroupEntity);
  }
}