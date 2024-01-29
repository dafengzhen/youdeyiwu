package com.youdeyiwu.repository.point;

import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import com.youdeyiwu.model.entity.point.PointAutoRuleEntity;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * point auto rule.
 *
 * @author dafengzhen
 */
public interface PointAutoRuleRepository extends CrudRepository<PointAutoRuleEntity, Long>,
    PagingAndSortingRepository<PointAutoRuleEntity, Long> {

  /**
   * findByAutoRuleName.
   *
   * @param autoRuleName autoRuleName
   * @return Optional
   */
  Optional<PointAutoRuleEntity> findByAutoRuleName(AutoRuleNameEnum autoRuleName);
}
