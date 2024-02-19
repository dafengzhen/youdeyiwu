package com.youdeyiwu.repository.point;

import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.model.entity.point.PointRuleEntity;
import java.util.Optional;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * point rule.
 *
 * @author dafengzhen
 */
public interface PointRuleRepository extends JpaRepositoryImplementation<PointRuleEntity, Long> {

  /**
   * findByRuleName.
   *
   * @param ruleName ruleName
   * @return Optional
   */
  Optional<PointRuleEntity> findByRuleName(RuleNameEnum ruleName);
}
