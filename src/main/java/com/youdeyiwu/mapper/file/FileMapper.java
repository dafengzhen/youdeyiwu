package com.youdeyiwu.mapper.file;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.file.FileEntity;
import com.youdeyiwu.model.vo.file.FileEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/**
 * file.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface FileMapper {

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return FileEntityVo
   */
  @Mapping(target = "user", ignore = true)
  FileEntityVo entityToVo(FileEntity entity);
}
