package com.youdeyiwu.repository.point.impl;

import com.youdeyiwu.repository.point.CustomizedPointHistoryRepository;
import jakarta.persistence.EntityManager;
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
  public Integer findPointHistoryCountParity() {
    return entityManager.createQuery(
            "select case when mod(count(*), 2) = 0 then 0 else 1 end from UserEntity",
            Integer.class
        )
        .getSingleResult();
  }
}
