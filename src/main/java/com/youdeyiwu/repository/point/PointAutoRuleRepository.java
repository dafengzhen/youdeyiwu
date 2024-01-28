package com.youdeyiwu.repository.point;

import com.youdeyiwu.model.entity.point.PointAutoRuleEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * point auto rule.
 *
 * @author dafengzhen
 */
public interface PointAutoRuleRepository extends CrudRepository<PointAutoRuleEntity, Long>,
    PagingAndSortingRepository<PointAutoRuleEntity, Long> {

}
