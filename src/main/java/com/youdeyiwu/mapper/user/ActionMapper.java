package com.youdeyiwu.mapper.user;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.mapper.other.StringMapper;
import com.youdeyiwu.model.dto.user.CreateActionDto;
import com.youdeyiwu.model.dto.user.UpdateActionDto;
import com.youdeyiwu.model.entity.user.ActionEntity;
import com.youdeyiwu.model.vo.user.ActionEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

/**
 * action.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class, uses = StringMapper.class)
public interface ActionMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "name", qualifiedByName = "trim")
  @Mapping(target = "alias", qualifiedByName = "trim")
  void dtoToEntity(CreateActionDto dto, @MappingTarget ActionEntity entity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "name", qualifiedByName = "trim")
  @Mapping(target = "alias", qualifiedByName = "trim")
  @Mapping(target = "menu", ignore = true)
  @Mapping(target = "submenu", ignore = true)
  void dtoToEntity(UpdateActionDto dto, @MappingTarget ActionEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return ActionEntityVo
   */
  @Mapping(target = "menu", ignore = true)
  @Mapping(target = "submenu", ignore = true)
  @Mapping(target = "role", ignore = true)
  ActionEntityVo entityToVo(ActionEntity entity);
}
