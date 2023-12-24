package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.SectionEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * section.
 *
 * @author dafengzhen
 */
public interface SectionRepository extends CrudRepository<SectionEntity, Long>,
    PagingAndSortingRepository<SectionEntity, Long>, CustomizedSectionRepository {

}
