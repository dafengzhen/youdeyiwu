package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.SectionGroupEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * section group.
 *
 * @author dafengzhen
 */
public interface SectionGroupRepository extends CrudRepository<SectionGroupEntity, Long>,
    PagingAndSortingRepository<SectionGroupEntity, Long> {

}
