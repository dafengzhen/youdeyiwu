package com.youdeyiwu.mapper.user;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.mapper.other.StringMapper;
import com.youdeyiwu.model.dto.user.CreateSubmenuDto;
import com.youdeyiwu.model.dto.user.UpdateSubmenuDto;
import com.youdeyiwu.model.entity.user.SubmenuEntity;
import com.youdeyiwu.model.vo.user.SubmenuEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

/**
 * submenu.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class, uses = StringMapper.class)
public interface SubmenuMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "name", qualifiedByName = "trim")
  void dtoToEntity(CreateSubmenuDto dto, @MappingTarget SubmenuEntity entity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  @Mapping(target = "name", qualifiedByName = "trim")
  @Mapping(target = "menu", ignore = true)
  @Mapping(target = "actions", ignore = true)
  void dtoToEntity(UpdateSubmenuDto dto, @MappingTarget SubmenuEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return SubmenuEntityVo
   */
  @Mapping(target = "menu", ignore = true)
  @Mapping(target = "actions", ignore = true)
  @Mapping(target = "roles", ignore = true)
  SubmenuEntityVo entityToVo(SubmenuEntity entity);
}
