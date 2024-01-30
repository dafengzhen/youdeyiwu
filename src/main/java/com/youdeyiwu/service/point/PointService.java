package com.youdeyiwu.service.point;

import com.youdeyiwu.model.dto.point.CreatePointAutoRuleDto;
import com.youdeyiwu.model.dto.point.CreatePointRuleDto;
import com.youdeyiwu.model.entity.point.PointAutoRuleEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointRuleEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.point.PointAutoRuleEntityVo;
import com.youdeyiwu.model.vo.point.PointHistoryEntityVo;
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
   * create.
   *
   * @param dto dto
   * @return PointAutoRuleEntity
   */
  PointAutoRuleEntity create(CreatePointAutoRuleDto dto);

  /**
   * create.
   *
   * @param dto dto
   * @return PointRuleEntity
   */
  PointRuleEntity create(CreatePointRuleDto dto);

  /**
   * query auto rules.
   *
   * @return List
   */
  List<PointAutoRuleEntityVo> queryAutoRules();

  /**
   * query rules.
   *
   * @return List
   */
  List<PointRuleEntityVo> queryRules();

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