package com.youdeyiwu.service.point;

import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.model.dto.point.UpdatePointDto;
import com.youdeyiwu.model.entity.point.PointEntity;
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
   * @param pointEntity  pointEntity
   * @param pointValue   pointValue
   * @param sign         sign
   * @param autoRuleName autoRuleName
   * @param ruleName     ruleName
   * @param reason       reason
   */
  void create(
      PointEntity pointEntity,
      Integer pointValue,
      SignEnum sign,
      AutoRuleNameEnum autoRuleName,
      RuleNameEnum ruleName,
      String reason
  );

  /**
   * update.
   *
   * @param pointEntity pointEntity
   * @param dto         dto
   * @return PointEntity
   */
  PointEntity update(PointEntity pointEntity, UpdatePointDto dto);
}