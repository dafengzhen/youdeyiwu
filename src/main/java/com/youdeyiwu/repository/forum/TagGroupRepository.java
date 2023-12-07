package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.TagGroupEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * tag group.
 *
 * @author dafengzhen
 */
public interface TagGroupRepository extends CrudRepository<TagGroupEntity, Long>,
    PagingAndSortingRepository<TagGroupEntity, Long> {

}
