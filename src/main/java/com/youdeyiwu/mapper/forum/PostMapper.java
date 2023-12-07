package com.youdeyiwu.mapper.forum;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.dto.forum.CreatePostDto;
import com.youdeyiwu.model.dto.forum.UpdatePostDto;
import com.youdeyiwu.model.entity.forum.PostBadgeEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostFavoriteEntity;
import com.youdeyiwu.model.entity.forum.PostImageEntity;
import com.youdeyiwu.model.vo.forum.PostBadgeEntityVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.PostFavoriteEntityVo;
import com.youdeyiwu.model.vo.forum.PostImageEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

/**
 * post.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface PostMapper {

  /**
   * entityToEntity.
   *
   * @param postEntity         postEntity
   * @param postFavoriteEntity postFavoriteEntity
   */
  @Mapping(target = "post", ignore = true)
  @Mapping(target = "user", ignore = true)
  void entityToEntity(PostEntity postEntity, @MappingTarget PostFavoriteEntity postFavoriteEntity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "cover", ignore = true)
  @Mapping(target = "contentLink", ignore = true)
  @Mapping(target = "content", ignore = true)
  @Mapping(target = "tags", ignore = true)
  void dtoToEntity(CreatePostDto dto, @MappingTarget PostEntity entity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "cover", ignore = true)
  @Mapping(target = "contentLink", ignore = true)
  @Mapping(target = "content", ignore = true)
  @Mapping(target = "tags", ignore = true)
  void dtoToEntity(UpdatePostDto dto, @MappingTarget PostEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PostEntityVo
   */
  @Mapping(target = "badges", ignore = true)
  @Mapping(target = "images", ignore = true)
  @Mapping(target = "allows", ignore = true)
  @Mapping(target = "blocks", ignore = true)
  @Mapping(target = "section", ignore = true)
  @Mapping(target = "tags", ignore = true)
  @Mapping(target = "user", ignore = true)
  @Mapping(target = "comments", ignore = true)
  PostEntityVo entityToVo(PostEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PostFavoriteEntityVo
   */
  @Mapping(target = "postId", ignore = true)
  PostFavoriteEntityVo entityToVo(PostFavoriteEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PostImageEntityVo
   */
  PostImageEntityVo entityToVo(PostImageEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PostBadgeEntityVo
   */
  PostBadgeEntityVo entityToVo(PostBadgeEntity entity);
}
