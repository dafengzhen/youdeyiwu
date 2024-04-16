package com.youdeyiwu.repository.point;

import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * point history.
 *
 * @author dafengzhen
 */
public interface PointHistoryRepository
    extends JpaRepositoryImplementation<PointHistoryEntity, Long>,
    CustomizedPointHistoryRepository {

  /**
   * findAllByUser.
   *
   * @param user     user
   * @param pageable pageable
   * @return Page
   */
  Page<PointHistoryEntity> findAllByUser(UserEntity user, Pageable pageable);
}
