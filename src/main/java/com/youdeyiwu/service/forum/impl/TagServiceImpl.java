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
import com.youdeyiwu.tool.I18nTool;
import java.util.List;
import java.util.Objects;
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

  private final I18nTool i18nTool;

  @Transactional
  @Override
  public TagEntity create(CreateTagDto dto, boolean throwErrorIfDuplicate) {
    String name = dto.name().trim();
    if (tagRepository.existsByName(name)) {
      if (throwErrorIfDuplicate) {
        throw new CustomException(i18nTool.getMessage("tag.name.duplicate"));
      } else {
        return tagRepository.findByName(name);
      }
    }

    TagEntity tagEntity = new TagEntity();
    tagEntity.setName(name);
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
      if (tagEntity.getName().equalsIgnoreCase(name)) {
        tagEntity.setName(name);
      } else if (tagRepository.existsByName(name)) {
        throw new CustomException(i18nTool.getMessage("tag.name.duplicate"));
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
  public List<TagEntityVo> selectAll() {
    return StreamSupport.stream(
            tagRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id"))
                .spliterator(),
            false
        )
        .map(tagMapper::entityToVo)
        .toList();
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