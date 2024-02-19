package com.youdeyiwu.repository.point;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import com.youdeyiwu.model.entity.point.PointPermissionRuleEntity;
import java.util.Optional;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * point permission rule.
 *
 * @author dafengzhen
 */
public interface PointPermissionRuleRepository extends JpaRepositoryImplementation<PointPermissionRuleEntity, Long> {

  /**
   * findByPermissionRuleName.
   *
   * @param permissionRuleName permissionRuleName
   * @return Optional
   */
  Optional<PointPermissionRuleEntity> findByPermissionRuleName(PermissionRuleNameEnum permissionRuleName);
}
