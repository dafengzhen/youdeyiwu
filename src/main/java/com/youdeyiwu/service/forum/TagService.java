package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreateTagDto;
import com.youdeyiwu.model.dto.forum.UpdateTagDto;
import com.youdeyiwu.model.entity.forum.TagEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.TagEntityVo;
import java.util.Set;
import org.springframework.data.domain.Pageable;

/**
 * tag.
 *
 * @author dafengzhen
 */
public interface TagService {

  /**
   * create.
   *
   * @param dto                   dto
   * @param throwErrorIfDuplicate throwErrorIfDuplicate
   * @return TagEntity
   */
  TagEntity create(CreateTagDto dto, boolean throwErrorIfDuplicate);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdateTagDto dto);

  /**
   * queryAll.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<TagEntityVo> queryAll(Pageable pageable);

  /**
   * select all.
   *
   * @return Set
   */
  Set<TagEntityVo> selectAll();

  /**
   * query.
   *
   * @param id id
   * @return TagEntityVo
   */
  TagEntityVo query(Long id);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}