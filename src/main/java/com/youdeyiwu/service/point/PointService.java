package com.youdeyiwu.service.point;

import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
import org.springframework.data.domain.Pageable;

/**
 * point.
 *
 * @author dafengzhen
 */
public interface PointService {

  /**
   * queryAll.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<PointHistoryEntityVo> queryAll(Pageable pageable);

  /**
   * find point by user id.
   *
   * @param userId userId
   * @return PointEntity
   */
  PointEntity findPointByUserId(Long userId);

  /**
   * find point by user entity.
   *
   * @param userEntity userEntity
   * @return PointEntity
   */
  PointEntity findPointByUserEntity(UserEntity userEntity);
}