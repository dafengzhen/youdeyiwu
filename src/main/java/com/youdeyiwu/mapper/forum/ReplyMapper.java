package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.vo.forum.QuoteReplyEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/**
 * reply.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface ReplyMapper {

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return QuoteReplyEntityVo
   */
  @Mapping(target = "comment", ignore = true)
  @Mapping(target = "quoteReply", ignore = true)
  @Mapping(target = "user", ignore = true)
  QuoteReplyEntityVo entityToVo(QuoteReplyEntity entity);
}
