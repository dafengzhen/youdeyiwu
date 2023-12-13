package com.youdeyiwu.repository.user;

import com.youdeyiwu.model.entity.user.MenuEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * menu.
 *
 * @author dafengzhen
 */
public interface MenuRepository extends CrudRepository<MenuEntity, Long>,
    PagingAndSortingRepository<MenuEntity, Long> {

}
