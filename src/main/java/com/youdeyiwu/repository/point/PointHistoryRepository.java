package com.youdeyiwu.repository.point;

import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * point history.
 *
 * @author dafengzhen
 */
public interface PointHistoryRepository extends CrudRepository<PointHistoryEntity, Long>,
    PagingAndSortingRepository<PointHistoryEntity, Long>, CustomizedPointHistoryRepository {

}
