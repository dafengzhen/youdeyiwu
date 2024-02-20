package com.youdeyiwu.repository.point;

import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * point history.
 *
 * @author dafengzhen
 */
public interface PointHistoryRepository
    extends JpaRepositoryImplementation<PointHistoryEntity, Long>,
    CustomizedPointHistoryRepository {

}
