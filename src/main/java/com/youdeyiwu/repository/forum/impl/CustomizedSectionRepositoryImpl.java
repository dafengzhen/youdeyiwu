package com.youdeyiwu.repository.forum.impl;

import com.youdeyiwu.enums.forum.SectionStateEnum;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.forum.CustomizedSectionRepository;
import jakarta.persistence.EntityManager;
import java.util.List;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 * section.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Repository
public class CustomizedSectionRepositoryImpl implements CustomizedSectionRepository {

  private final EntityManager entityManager;

  @Override
  public List<SectionEntity> findAll(
      String accessKey,
      Boolean isAnonymous,
      UserEntity user,
      UserEntity root
  ) {
    if (Boolean.TRUE.equals(isAnonymous)) {
      return entityManager.createQuery(
              """
                  select s from SectionEntity s
                  where :show member of s.states
                  or (:lock member of s.states and s.accessKey = :accessKey)
                  order by s.sort desc, s.id desc
                  """,
              SectionEntity.class
          )
          .setParameter("show", SectionStateEnum.SHOW)
          .setParameter("lock", SectionStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .getResultList();
    } else if (Objects.nonNull(root)) {
      return queryRootUserSections();
    } else if (Objects.nonNull(user)) {
      return entityManager.createQuery(
              """
                  select s from SectionEntity s
                  where :show member of s.states
                  or (:hide member of s.states and :user member of s.admins)
                  or (:lock member of s.states and s.accessKey = :accessKey)
                  or (:block member of s.states and :user member of s.blocks)
                  order by s.sort desc, s.id desc
                  """,
              SectionEntity.class
          )
          .setParameter("show", SectionStateEnum.SHOW)
          .setParameter("hide", SectionStateEnum.HIDE)
          .setParameter("lock", SectionStateEnum.LOCK)
          .setParameter("accessKey", accessKey)
          .setParameter("block", SectionStateEnum.BLOCK)
          .setParameter("user", user)
          .getResultList();
    }
    return queryRootUserSections();
  }

  /**
   * query root user sections.
   *
   * @return List
   */
  private List<SectionEntity> queryRootUserSections() {
    return entityManager.createQuery(
            """
                select s from SectionEntity s
                order by s.sort desc, s.id desc
                """,
            SectionEntity.class
        )
        .getResultList();
  }
}
