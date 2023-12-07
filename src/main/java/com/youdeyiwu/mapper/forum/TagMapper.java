package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.dto.forum.CreateTagDto;
import com.youdeyiwu.model.entity.forum.TagEntity;
import com.youdeyiwu.model.vo.forum.TagEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.MappingTarget;

/**
 * tag.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface TagMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  void dtoToEntity(CreateTagDto dto, @MappingTarget TagEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return TagEntityVo
   */
  TagEntityVo entityToVo(TagEntity entity);
}
