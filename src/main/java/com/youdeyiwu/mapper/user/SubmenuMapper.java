package com.youdeyiwu.mapper.user;

import com.youdeyiwu.config.MapperTemplateConfig;
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
@Mapper(config = MapperTemplateConfig.class)
public interface SubmenuMapper {

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  void dtoToEntity(CreateSubmenuDto dto, @MappingTarget SubmenuEntity entity);

  /**
   * dtoToEntity.
   *
   * @param dto    dto
   * @param entity entity
   */
  void dtoToEntity(UpdateSubmenuDto dto, @MappingTarget SubmenuEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return SubmenuEntityVo
   */
  @Mapping(target = "menu", ignore = true)
  SubmenuEntityVo entityToVo(SubmenuEntity entity);

}
