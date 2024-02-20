package com.youdeyiwu.mapper.user;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.mapper.other.StringMapper;
import com.youdeyiwu.model.dto.user.CreateMenuDto;
import com.youdeyiwu.model.dto.user.UpdateMenuDto;
import com.youdeyiwu.model.entity.user.MenuEntity;
import com.youdeyiwu.model.vo.user.MenuEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

/**
 * menu.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class, uses = StringMapper.class)
public interface MenuMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "name", qualifiedByName = "trim")
  void dtoToEntity(CreateMenuDto dto, @MappingTarget MenuEntity entity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "name", qualifiedByName = "trim")
  @Mapping(target = "submenus", ignore = true)
  @Mapping(target = "actions", ignore = true)
  void dtoToEntity(UpdateMenuDto dto, @MappingTarget MenuEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return RoleEntityVo
   */
  @Mapping(target = "submenus", ignore = true)
  @Mapping(target = "actions", ignore = true)
  @Mapping(target = "roles", ignore = true)
  MenuEntityVo entityToVo(MenuEntity entity);

}
