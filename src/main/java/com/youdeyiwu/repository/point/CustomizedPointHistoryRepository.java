package com.youdeyiwu.repository.point;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import java.util.Optional;

/**
 * point history.
 *
 * @author dafengzhen
 */
public interface CustomizedPointHistoryRepository {

  /**
   * findLatestPointsHistoryByUserIdAndRuleName.
   *
   * @param userId   userId
   * @param ruleName ruleName
   * @return Optional
   */
  Optional<PointHistoryEntity> findLatestPointsHistoryByUserIdAndRuleName(
      Long userId,
      RuleNameEnum ruleName
  );

  /**
   * findLatestPointsHistoryByUserIdAndPermissionRuleName.
   *
   * @param userId             userId
   * @param permissionRuleName permissionRuleName
   * @return Optional
   */
  Optional<PointHistoryEntity> findLatestPointsHistoryByUserIdAndPermissionRuleName(
      Long userId,
      PermissionRuleNameEnum permissionRuleName
  );
}
