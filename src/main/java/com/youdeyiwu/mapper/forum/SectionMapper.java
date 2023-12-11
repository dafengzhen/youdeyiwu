package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.dto.forum.UpdateSectionDto;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.vo.forum.SectionEntityVo;
import com.youdeyiwu.model.vo.forum.SelectAllSectionEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

/**
 * section.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface SectionMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "cover", ignore = true)
  @Mapping(target = "content", ignore = true)
  void dtoToEntity(UpdateSectionDto dto, @MappingTarget SectionEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return SectionEntityVo
   */
  @Mapping(target = "admins", ignore = true)
  @Mapping(target = "allows", ignore = true)
  @Mapping(target = "blocks", ignore = true)
  @Mapping(target = "tagGroups", ignore = true)
  @Mapping(target = "tags", ignore = true)
  @Mapping(target = "sectionGroups", ignore = true)
  SectionEntityVo entityToVo(SectionEntity entity);

  /**
   * entityToSelectAllVo.
   *
   * @param entity entity
   * @return SelectAllSectionEntityVo
   */
  SelectAllSectionEntityVo entityToSelectAllVo(SectionEntity entity);
}