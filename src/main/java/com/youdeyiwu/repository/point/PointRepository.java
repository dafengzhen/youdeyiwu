package com.youdeyiwu.repository.point;

import com.youdeyiwu.model.entity.point.PointEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * point.
 *
 * @author dafengzhen
 */
public interface PointRepository extends CrudRepository<PointEntity, Long>,
    PagingAndSortingRepository<PointEntity, Long> {

}
