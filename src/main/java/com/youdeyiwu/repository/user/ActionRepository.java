package com.youdeyiwu.repository.user;

import com.youdeyiwu.model.entity.user.ActionEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * action.
 *
 * @author dafengzhen
 */
public interface ActionRepository extends CrudRepository<ActionEntity, Long>,
    PagingAndSortingRepository<ActionEntity, Long> {

}
