package com.youdeyiwu.repository.user;

import com.youdeyiwu.model.entity.user.PermissionEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * permission.
 *
 * @author dafengzhen
 */
public interface PermissionRepository extends CrudRepository<PermissionEntity, Long>,
    PagingAndSortingRepository<PermissionEntity, Long> {

}
