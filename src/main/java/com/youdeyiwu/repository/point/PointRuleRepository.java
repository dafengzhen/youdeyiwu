package com.youdeyiwu.repository.point;

import com.youdeyiwu.model.entity.point.PointRuleEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * point rule.
 *
 * @author dafengzhen
 */
public interface PointRuleRepository extends CrudRepository<PointRuleEntity, Long>,
    PagingAndSortingRepository<PointRuleEntity, Long> {

}
