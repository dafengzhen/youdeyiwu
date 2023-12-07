package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.vo.forum.CommentEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/**
 * comment.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface CommentMapper {

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return CommentEntityVo
   */
  @Mapping(target = "user", ignore = true)
  CommentEntityVo entityToVo(CommentEntity entity);
}
