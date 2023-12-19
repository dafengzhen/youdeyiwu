package com.youdeyiwu.repository.message.impl;

import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.entity.message.GlobalMessageUserEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.message.CustomizedMessageRepository;
import jakarta.persistence.EntityManager;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 * message.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Repository
public class CustomizedMessageRepositoryImpl implements CustomizedMessageRepository {

  private final EntityManager entityManager;

  @Override
  public Page<GlobalMessageUserEntity> findAllByReceiver(
      UserEntity receiver,
      PaginationPositionDto position
  ) {
    List<GlobalMessageUserEntity> entities = entityManager.createQuery(
            """
                 select g.globalMessage.id from GlobalMessageUserEntity g
                  left join g.user gu
                  where gu.id = :receiverId
                  order by g.state, g.globalMessage.sort desc, g.globalMessage.id desc
                """,
            GlobalMessageUserEntity.class
        )
        .setParameter("receiverId", receiver.getId())
        .setFirstResult(position.firstResult())
        .setMaxResults(position.maxResults())
        .getResultList();

    Long totalSize = entityManager.createQuery(
            """
                select count(g.globalMessage.id) from GlobalMessageUserEntity g
                left join g.user gu
                where gu.id = :receiverId
                """,
            Long.class
        )
        .setParameter("receiverId", receiver.getId())
        .getSingleResult();

    return new PageImpl<>(entities, position.pageable(), totalSize);
  }
}
