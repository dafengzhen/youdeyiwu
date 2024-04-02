package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreateSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateAdminsSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateCreatePostGuideSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateStatesSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateTagGroupsSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsSectionDto;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.vo.CoverVo;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.SectionEntityVo;
import jakarta.validation.Valid;
import java.util.List;
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
   * update create post guide.
   *
   * @param id  id
   * @param dto dto
   */
  void updateCreatePostGuide(Long id, UpdateCreatePostGuideSectionDto dto);

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
   * @param id         id
   * @param sectionKey sectionKey
   * @return SectionEntityVo
   */
  SectionEntityVo queryDetails(Long id, String sectionKey);

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
   * @param sectionKey sectionKey
   * @return List
   */
  List<SectionEntityVo> selectAll(String sectionKey);

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