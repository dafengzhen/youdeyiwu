package com.youdeyiwu.repository.point;

import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.model.entity.point.PointRuleEntity;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * point rule.
 *
 * @author dafengzhen
 */
public interface PointRuleRepository extends CrudRepository<PointRuleEntity, Long>,
    PagingAndSortingRepository<PointRuleEntity, Long> {

  /**
   * findByRuleName.
   *
   * @param ruleName ruleName
   * @return Optional
   */
  Optional<PointRuleEntity> findByRuleName(RuleNameEnum ruleName);
}
