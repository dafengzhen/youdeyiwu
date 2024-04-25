package com.youdeyiwu.service.data;

import com.youdeyiwu.model.dto.data.CreatePostImportDto;
import com.youdeyiwu.model.dto.data.CreateSectionGroupImportDto;
import com.youdeyiwu.model.dto.data.CreateSectionImportDto;
import com.youdeyiwu.model.dto.data.CreateTagGroupImportDto;
import com.youdeyiwu.model.dto.data.CreateTagImportDto;
import com.youdeyiwu.model.dto.data.CreateUserImportDto;
import java.util.List;

/**
 * import.
 *
 * @author dafengzhen
 */
public interface ImportService {

  /**
   * create users.
   *
   * @param dto dto
   * @return List
   */
  List<CreateUserImportDto.UserImportVo> createUsers(CreateUserImportDto dto);

  /**
   * create sections.
   *
   * @param dto dto
   * @return List
   */
  List<CreateSectionImportDto.SectionImportVo> createSections(CreateSectionImportDto dto);

  /**
   * create section groups.
   *
   * @param dto dto
   * @return List
   */
  List<CreateSectionGroupImportDto.SectionGroupImportVo> createSectionGroups(CreateSectionGroupImportDto dto);

  /**
   * create tag.
   *
   * @param dto dto
   * @return List
   */
  List<CreateTagImportDto.TagImportVo> createTags(CreateTagImportDto dto);

  /**
   * create tag groups.
   *
   * @param dto dto
   * @return List
   */
  List<CreateTagGroupImportDto.TagGroupImportVo> createTagGroups(CreateTagGroupImportDto dto);

  /**
   * create post.
   *
   * @param dto dto
   * @return List
   */
  List<CreatePostImportDto.PostImportVo> createPosts(CreatePostImportDto dto);
}