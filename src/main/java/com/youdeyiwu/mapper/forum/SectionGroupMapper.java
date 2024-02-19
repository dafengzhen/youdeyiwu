package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.forum.SectionGroupEntity;
import com.youdeyiwu.model.vo.forum.SectionGroupEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/**
 * section group.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface SectionGroupMapper {

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return SectionGroupEntityVo
   */
  @Mapping(target = "sections", ignore = true)
  SectionGroupEntityVo entityToVo(SectionGroupEntity entity);
}
