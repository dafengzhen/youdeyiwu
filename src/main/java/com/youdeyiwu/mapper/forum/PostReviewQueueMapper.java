package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.forum.PostReviewQueueEntity;
import com.youdeyiwu.model.vo.forum.PostReviewQueueEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/**
 * post review queue.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface PostReviewQueueMapper {

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PostFavoriteEntityVo
   */
  @Mapping(target = "receiver", ignore = true)
  @Mapping(target = "post", ignore = true)
  PostReviewQueueEntityVo entityToVo(PostReviewQueueEntity entity);
}
