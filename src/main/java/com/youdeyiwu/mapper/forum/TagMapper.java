package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.forum.TagEntity;
import com.youdeyiwu.model.vo.forum.TagEntityVo;
import org.mapstruct.Mapper;

/**
 * tag.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface TagMapper {

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return TagEntityVo
   */
  TagEntityVo entityToVo(TagEntity entity);
}
