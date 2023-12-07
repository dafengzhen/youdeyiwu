package com.youdeyiwu.repository.user;

import com.youdeyiwu.model.entity.user.RoleEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * role.
 *
 * @author dafengzhen
 */
public interface RoleRepository extends CrudRepository<RoleEntity, Long>,
    PagingAndSortingRepository<RoleEntity, Long> {

}
