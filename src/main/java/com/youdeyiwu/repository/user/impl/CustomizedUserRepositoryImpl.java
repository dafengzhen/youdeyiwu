package com.youdeyiwu.repository.user.impl;

import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.user.UsersCountByDateVo;
import com.youdeyiwu.repository.user.CustomizedUserRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.Tuple;
import java.sql.Date;
import java.time.OffsetDateTime;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 * user.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Repository
public class CustomizedUserRepositoryImpl implements CustomizedUserRepository {

  private final EntityManager entityManager;

  @Override
  public List<UsersCountByDateVo> getUsersCountByDate(
      OffsetDateTime startDate,
      OffsetDateTime endDate
  ) {
    List<Tuple> tuples = entityManager.createQuery(
            """
                select date(u.createdOn) as date, count(id) as count from UserEntity u
                where u.createdOn >= :startDate and u.createdOn <= :endDate
                group by date(u.createdOn)
                """,
            Tuple.class
        )
        .setParameter("startDate", startDate)
        .setParameter("endDate", endDate)
        .getResultList();

    return tuples.stream()
        .map(tuple -> {
          UsersCountByDateVo vo = new UsersCountByDateVo();
          vo.setDate(((Date) tuple.get(0)).toLocalDate());
          vo.setCount((Long) tuple.get(1));
          return vo;
        })
        .toList();
  }

  @Override
  public UserEntity findAllRoleById(Long id) {
    return entityManager.createQuery(
            """
                  select u from UserEntity u left join fetch u.roles where u.id = :id
                """,
            UserEntity.class
        )
        .setParameter("id", id)
        .getSingleResult();
  }
}
