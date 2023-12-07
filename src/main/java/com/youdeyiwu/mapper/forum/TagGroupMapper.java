package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.dto.forum.CreateTagGroupDto;
import com.youdeyiwu.model.entity.forum.TagGroupEntity;
import com.youdeyiwu.model.vo.forum.TagGroupEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

/**
 * tag group.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface TagGroupMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  void dtoToEntity(CreateTagGroupDto dto, @MappingTarget TagGroupEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return TagGroupEntityVo
   */
  @Mapping(target = "tags", ignore = true)
  TagGroupEntityVo entityToVo(TagGroupEntity entity);
}
