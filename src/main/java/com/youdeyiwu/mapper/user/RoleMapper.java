package com.youdeyiwu.mapper.user;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.mapper.other.StringMapper;
import com.youdeyiwu.model.dto.user.CreateRoleDto;
import com.youdeyiwu.model.dto.user.UpdateRoleDto;
import com.youdeyiwu.model.entity.user.RoleEntity;
import com.youdeyiwu.model.vo.user.RoleEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

/**
 * role.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class, uses = StringMapper.class)
public interface RoleMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "name", qualifiedByName = "trim")
  @Mapping(target = "overview", qualifiedByName = "trim")
  void dtoToEntity(CreateRoleDto dto, @MappingTarget RoleEntity entity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "name", qualifiedByName = "trim")
  @Mapping(target = "overview", qualifiedByName = "trim")
  void dtoToEntity(UpdateRoleDto dto, @MappingTarget RoleEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return RoleEntityVo
   */
  @Mapping(target = "permissions", ignore = true)
  RoleEntityVo entityToVo(RoleEntity entity);

}
