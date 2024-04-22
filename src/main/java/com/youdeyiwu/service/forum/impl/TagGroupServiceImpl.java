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
import java.util.List;
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
    tagGroupEntity.setName(dto.name().trim());
    return tagGroupRepository.save(tagGroupEntity);
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
      tagGroupEntity.setName(dto.name().trim());
    }

    if (Objects.nonNull(dto.sort())) {
      tagGroupEntity.setSort(dto.sort());
    }
  }

  @Override
  public PageVo<TagGroupEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(tagGroupRepository.findAll(pageable).map(tagGroupEntity -> {
      TagGroupEntityVo vo = tagGroupMapper.entityToVo(tagGroupEntity);
      setTags(tagGroupEntity, vo);
      return vo;
    }));
  }

  @Override
  public List<TagGroupEntityVo> selectAll() {
    return tagGroupRepository.findAll().stream()
        .map(tagGroupEntity -> {
          TagGroupEntityVo vo = tagGroupMapper.entityToVo(tagGroupEntity);
          setTags(tagGroupEntity, vo);
          return vo;
        })
        .toList();
  }

  @Override
  public TagGroupEntityVo query(Long id) {
    TagGroupEntity tagGroupEntity = tagGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);
    TagGroupEntityVo vo = tagGroupMapper.entityToVo(tagGroupEntity);
    setTags(tagGroupEntity, vo);
    return vo;
  }

  @Transactional
  @Override
  public void delete(Long id) {
    TagGroupEntity tagGroupEntity = tagGroupRepository.findById(id)
        .orElseThrow(TagGroupNotFoundException::new);
    tagGroupRepository.delete(tagGroupEntity);
  }

  /**
   * set tags.
   *
   * @param tagGroupEntity tagGroupEntity
   * @param vo             vo
   */
  private void setTags(TagGroupEntity tagGroupEntity, TagGroupEntityVo vo) {
    vo.setTags(
        tagGroupEntity.getTags()
            .stream()
            .map(tagMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }
}