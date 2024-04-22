package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreateTagGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateTagGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsTagGroupDto;
import com.youdeyiwu.model.entity.forum.TagGroupEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.TagGroupEntityVo;
import java.util.List;
import org.springframework.data.domain.Pageable;

/**
 * tag group.
 *
 * @author dafengzhen
 */
public interface TagGroupService {

  /**
   * create.
   *
   * @param dto dto
   * @return TagGroupEntity
   */
  TagGroupEntity create(CreateTagGroupDto dto);

  /**
   * update tags.
   *
   * @param id  id
   * @param dto dto
   */
  void updateTags(Long id, UpdateTagsTagGroupDto dto);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdateTagGroupDto dto);

  /**
   * queryAll.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<TagGroupEntityVo> queryAll(Pageable pageable);

  /**
   * select all.
   *
   * @return List
   */
  List<TagGroupEntityVo> selectAll();

  /**
   * query.
   *
   * @param id id
   * @return TagGroupEntity
   */
  TagGroupEntityVo query(Long id);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}