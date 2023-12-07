package com.youdeyiwu.mapper.user;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/**
 * user.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface UserMapper {

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return UserEntityVo
   */
  @Mapping(target = "roles", ignore = true)
  @Mapping(target = "posts", ignore = true)
  @Mapping(target = "favorites", ignore = true)
  UserEntityVo entityToVo(UserEntity entity);

}
