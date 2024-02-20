package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.forum.TagGroupEntity;
import com.youdeyiwu.model.vo.forum.TagGroupEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/**
 * tag group.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface TagGroupMapper {

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return TagGroupEntityVo
   */
  @Mapping(target = "tags", ignore = true)
  TagGroupEntityVo entityToVo(TagGroupEntity entity);
}
