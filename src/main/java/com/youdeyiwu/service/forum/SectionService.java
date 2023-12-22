package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreateSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateAdminsSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateStatesSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateTagGroupsSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsSectionDto;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.vo.CoverVo;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.SectionEntityVo;
import jakarta.validation.Valid;
import java.util.Set;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

/**
 * section.
 *
 * @author dafengzhen
 */
public interface SectionService {

  /**
   * create.
   *
   * @param dto dto
   * @return SectionEntity
   */
  SectionEntity create(@Valid CreateSectionDto dto);

  /**
   * upload cover.
   *
   * @param id   id
   * @param file file
   */
  void uploadCover(Long id, MultipartFile file);

  /**
   * update states.
   *
   * @param id  id
   * @param dto dto
   */
  void updateStates(Long id, UpdateStatesSectionDto dto);

  /**
   * update admins.
   *
   * @param id  id
   * @param dto dto
   */
  void updateAdmins(Long id, UpdateAdminsSectionDto dto);

  /**
   * update tags.
   *
   * @param id  id
   * @param dto dto
   */
  void updateTags(Long id, UpdateTagsSectionDto dto);

  /**
   * update tag groups.
   *
   * @param id  id
   * @param dto dto
   */
  void updateTagGroups(Long id, UpdateTagGroupsSectionDto dto);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdateSectionDto dto);

  /**
   * query details.
   *
   * @param id id
   * @return SectionEntityVo
   */
  SectionEntityVo queryDetails(Long id);

  /**
   * query cover.
   *
   * @param id id
   * @return CoverVo
   */
  CoverVo queryCover(Long id);

  /**
   * query.
   *
   * @param id id
   * @return SectionEntityVo
   */
  SectionEntityVo query(Long id);

  /**
   * select all.
   *
   * @return Set
   */
  Set<SectionEntityVo> selectAll();

  /**
   * queryAll.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<SectionEntityVo> queryAll(Pageable pageable);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}