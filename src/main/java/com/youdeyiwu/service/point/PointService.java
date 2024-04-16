package com.youdeyiwu.service.point;

import com.youdeyiwu.model.dto.point.SavePointPermissionRuleDto;
import com.youdeyiwu.model.dto.point.SavePointRuleDto;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
import com.youdeyiwu.model.vo.point.PointPermissionRuleEntityVo;
import com.youdeyiwu.model.vo.point.PointRuleEntityVo;
import java.util.List;
import org.springframework.data.domain.Pageable;

/**
 * point.
 *
 * @author dafengzhen
 */
public interface PointService {

  /**
   * save.
   *
   * @param dto dto
   */
  void save(SavePointRuleDto dto);

  /**
   * save.
   *
   * @param dto dto
   */
  void save(SavePointPermissionRuleDto dto);

  /**
   * query rules.
   *
   * @return List
   */
  List<PointRuleEntityVo> queryRules();

  /**
   * query permission rules.
   *
   * @return List
   */
  List<PointPermissionRuleEntityVo> queryPermissionRules();

  /**
   * queryAll.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<PointHistoryEntityVo> queryAll(Pageable pageable);

  /**
   * query all history by user id.
   *
   * @param userId   userId
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<PointHistoryEntityVo> queryAllHistoryByUserId(Long userId, Pageable pageable);

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