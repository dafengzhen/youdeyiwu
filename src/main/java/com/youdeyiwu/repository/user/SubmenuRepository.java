package com.youdeyiwu.repository.user;

import com.youdeyiwu.model.entity.user.SubmenuEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * submenu.
 *
 * @author dafengzhen
 */
public interface SubmenuRepository extends CrudRepository<SubmenuEntity, Long>,
    PagingAndSortingRepository<SubmenuEntity, Long> {

}
