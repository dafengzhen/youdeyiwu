package com.youdeyiwu.mapper.point;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import com.youdeyiwu.model.entity.point.PointPermissionRuleEntity;
import com.youdeyiwu.model.entity.point.PointRuleEntity;
import com.youdeyiwu.model.vo.point.PointEntityVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
import com.youdeyiwu.model.vo.point.PointPermissionRuleEntityVo;
import com.youdeyiwu.model.vo.point.PointRuleEntityVo;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

/**
 * point.
 *
 * @author dafengzhen
 */
@Mapper(config = MapperTemplateConfig.class)
public interface PointMapper {

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PointEntityVo
   */
  @Mapping(target = "user", ignore = true)
  PointEntityVo entityToVo(PointEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PointHistoryEntityVo
   */
  @Mapping(target = "user", ignore = true)
  PointHistoryEntityVo entityToVo(PointHistoryEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PointAutoRuleEntityVo
   */
  PointRuleEntityVo entityToVo(PointRuleEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PointRuleEntityVo
   */
  PointPermissionRuleEntityVo entityToVo(PointPermissionRuleEntity entity);

  /**
   * entityToEntity.
   *
   * @param pointEntity pointEntity
   * @return PointHistoryEntity
   */
  @Mapping(target = "id", ignore = true)
  @Mapping(target = "createdBy", ignore = true)
  @Mapping(target = "updatedBy", ignore = true)
  @Mapping(target = "createdOn", ignore = true)
  @Mapping(target = "updatedOn", ignore = true)
  @Mapping(target = "deleted", ignore = true)
  @Mapping(target = "version", ignore = true)
  @Mapping(target = "user", ignore = true)
  PointHistoryEntity entityToEntity(PointEntity pointEntity);
}
