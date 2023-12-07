package com.youdeyiwu.mapper.user;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.dto.user.CreatePermissionDto;
import com.youdeyiwu.model.dto.user.UpdatePermissionDto;
import com.youdeyiwu.model.entity.user.PermissionEntity;
import com.youdeyiwu.model.vo.user.PermissionEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

/**
 * permission.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface PermissionMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "matchers", ignore = true)
  void dtoToEntity(CreatePermissionDto dto, @MappingTarget PermissionEntity entity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "matchers", ignore = true)
  void dtoToEntity(UpdatePermissionDto dto, @MappingTarget PermissionEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PermissionEntityVo
   */
  @Mapping(target = "matcher", ignore = true)
  @Mapping(target = "matchers", ignore = true)
  @Mapping(target = "role", ignore = true)
  @Mapping(target = "roles", ignore = true)
  PermissionEntityVo entityToVo(PermissionEntity entity);
}
