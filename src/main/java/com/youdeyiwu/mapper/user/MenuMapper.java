package com.youdeyiwu.mapper.user;

import com.youdeyiwu.config.MapperTemplateConfig;
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
@Mapper(config = MapperTemplateConfig.class)
public interface MenuMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  void dtoToEntity(CreateMenuDto dto, @MappingTarget MenuEntity entity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "submenus", ignore = true)
  void dtoToEntity(UpdateMenuDto dto, @MappingTarget MenuEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return RoleEntityVo
   */
  @Mapping(target = "submenus", ignore = true)
  MenuEntityVo entityToVo(MenuEntity entity);

}
