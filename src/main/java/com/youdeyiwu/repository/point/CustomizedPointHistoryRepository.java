package com.youdeyiwu.repository.point;

import com.youdeyiwu.enums.point.AutoRuleNameEnum;
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
   * findLatestPointsHistoryByUserIdAndAutoRuleName.
   *
   * @param userId       userId
   * @param autoRuleName autoRuleName
   * @return Optional
   */
  Optional<PointHistoryEntity> findLatestPointsHistoryByUserIdAndAutoRuleName(
      Long userId,
      AutoRuleNameEnum autoRuleName
  );

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
}
