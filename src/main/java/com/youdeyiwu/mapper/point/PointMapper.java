package com.youdeyiwu.mapper.point;

import com.youdeyiwu.config.MapperTemplateConfig;
import com.youdeyiwu.model.entity.point.PointAutoRuleEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import com.youdeyiwu.model.entity.point.PointRuleEntity;
import com.youdeyiwu.model.vo.point.PointAutoRuleEntityVo;
import com.youdeyiwu.model.vo.point.PointEntityVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
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
  PointAutoRuleEntityVo entityToVo(PointAutoRuleEntity entity);

  /**
   * entityToVo.
   *
   * @param entity entity
   * @return PointRuleEntityVo
   */
  PointRuleEntityVo entityToVo(PointRuleEntity entity);
}
