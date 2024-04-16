package com.youdeyiwu.service.point;

import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import com.youdeyiwu.model.entity.user.UserEntity;

/**
 * point.
 *
 * @author dafengzhen
 */
public interface PointCoreService {

  /**
   * create.
   *
   * @param userEntity userEntity
   * @return PointEntity
   */
  PointEntity create(UserEntity userEntity);

  /**
   * create.
   *
   * @param pointEntity        pointEntity
   * @param pointHistoryEntity pointHistoryEntity
   */
  void create(PointEntity pointEntity, PointHistoryEntity pointHistoryEntity);

  /**
   * update.
   *
   * @param pointEntity pointEntity
   * @param dto         dto
   * @return PointEntity
   */
  PointEntity update(PointEntity pointEntity, UpdatePointDto dto);
}