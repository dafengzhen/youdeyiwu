package com.youdeyiwu.repository.point;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import com.youdeyiwu.model.entity.point.PointPermissionRuleEntity;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * point permission rule.
 *
 * @author dafengzhen
 */
public interface PointPermissionRuleRepository extends CrudRepository<PointPermissionRuleEntity, Long>,
    PagingAndSortingRepository<PointPermissionRuleEntity, Long> {

  /**
   * findByPermissionRuleName.
   *
   * @param permissionRuleName permissionRuleName
   * @return Optional
   */
  Optional<PointPermissionRuleEntity> findByPermissionRuleName(PermissionRuleNameEnum permissionRuleName);
}
