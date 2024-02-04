package com.youdeyiwu.repository.point.impl;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import com.youdeyiwu.repository.point.CustomizedPointHistoryRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.NoResultException;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 * point history.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Repository
public class CustomizedPointHistoryRepositoryImpl implements CustomizedPointHistoryRepository {

  private final EntityManager entityManager;

  @Override
  public Optional<PointHistoryEntity> findLatestPointsHistoryByUserIdAndRuleName(
      Long userId,
      RuleNameEnum ruleName
  ) {
    try {
      return Optional.of(
          entityManager.createQuery(
                  """
                      select ph from PointHistoryEntity ph
                      where ph.user.id = :userId and ph.ruleName = :ruleName
                      order by ph.id desc
                      limit 1
                      """,
                  PointHistoryEntity.class
              )
              .setParameter("userId", userId)
              .setParameter("ruleName", ruleName)
              .getSingleResult()
      );
    } catch (NoResultException e) {
      return Optional.empty();
    }
  }

  @Override
  public Optional<PointHistoryEntity> findLatestPointsHistoryByUserIdAndPermissionRuleName(
      Long userId,
      PermissionRuleNameEnum permissionRuleName
  ) {
    try {
      return Optional.of(
          entityManager.createQuery(
                  """
                      select ph from PointHistoryEntity ph
                      where ph.user.id = :userId and ph.permissionRuleName = :permissionRuleName
                      order by ph.id desc
                      limit 1
                      """,
                  PointHistoryEntity.class
              )
              .setParameter("userId", userId)
              .setParameter("permissionRuleName", permissionRuleName)
              .getSingleResult()
      );
    } catch (NoResultException e) {
      return Optional.empty();
    }
  }
}
