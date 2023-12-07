package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreateSectionGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionGroupDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionsSectionGroupDto;
import com.youdeyiwu.model.entity.forum.SectionGroupEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.SectionGroupEntityVo;
import java.util.Set;
import org.springframework.data.domain.Pageable;

/**
 * section group.
 *
 * @author dafengzhen
 */
public interface SectionGroupService {

  /**
   * create.
   *
   * @param dto dto
   * @return SectionGroupEntity
   */
  SectionGroupEntity create(CreateSectionGroupDto dto);

  /**
   * update sections.
   *
   * @param id  id
   * @param dto dto
   */
  void updateSections(Long id, UpdateSectionsSectionGroupDto dto);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdateSectionGroupDto dto);

  /**
   * queryAll.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<SectionGroupEntityVo> queryAll(Pageable pageable);

  /**
   * select all.
   *
   * @return Set
   */
  Set<SectionGroupEntityVo> selectAll();

  /**
   * query.
   *
   * @param id id
   * @return SectionGroupEntityVo
   */
  SectionGroupEntityVo query(Long id);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}