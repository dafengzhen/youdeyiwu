package com.youdeyiwu.service.forum.impl;

import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.TagNotFoundException;
import com.youdeyiwu.mapper.forum.TagMapper;
import com.youdeyiwu.model.dto.forum.CreateTagDto;
import com.youdeyiwu.model.dto.forum.UpdateTagDto;
import com.youdeyiwu.model.entity.forum.TagEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.TagEntityVo;
import com.youdeyiwu.repository.forum.TagRepository;
import com.youdeyiwu.service.forum.TagService;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * tag.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class TagServiceImpl implements TagService {

  private final TagRepository tagRepository;

  private final TagMapper tagMapper;

  @Transactional
  @Override
  public TagEntity create(CreateTagDto dto, boolean throwErrorIfDuplicate) {
    TagEntity tagEntity = new TagEntity();
    tagEntity.setCreatedOn(LocalDateTime.now());
    tagMapper.dtoToEntity(dto, tagEntity);

    if (tagRepository.existsByName(dto.name())) {
      if (throwErrorIfDuplicate) {
        throw new CustomException("The tag name already exists, cannot create duplicates");
      } else {
        return tagRepository.findByName(dto.name());
      }
    }

    tagRepository.save(tagEntity);
    return tagEntity;
  }

  @Transactional
  @Override
  public void update(Long id, UpdateTagDto dto) {
    TagEntity tagEntity = tagRepository.findById(id)
        .orElseThrow(TagNotFoundException::new);

    if (StringUtils.hasText(dto.name())) {
      String name = dto.name().trim();
      if (Boolean.TRUE.equals(tagRepository.existsByName(name))) {
        throw new CustomException("The tag name already exists, cannot create duplicates");
      } else {
        tagEntity.setName(name);
      }
    }

    if (Objects.nonNull(dto.sort())) {
      tagEntity.setSort(dto.sort());
    }
  }

  @Override
  public PageVo<TagEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(tagRepository.findAll(pageable).map(tagMapper::entityToVo));
  }

  @Override
  public Set<TagEntityVo> selectAll() {
    return StreamSupport.stream(
            tagRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id"))
                .spliterator(),
            false
        )
        .map(tagMapper::entityToVo)
        .collect(Collectors.toSet());
  }

  @Override
  public TagEntityVo query(Long id) {
    TagEntity tagEntity = tagRepository.findById(id)
        .orElseThrow(TagNotFoundException::new);
    return tagMapper.entityToVo(tagEntity);
  }

  @Transactional
  @Override
  public void delete(Long id) {
    TagEntity tagEntity = tagRepository.findById(id)
        .orElseThrow(TagNotFoundException::new);
    tagRepository.delete(tagEntity);
  }
}