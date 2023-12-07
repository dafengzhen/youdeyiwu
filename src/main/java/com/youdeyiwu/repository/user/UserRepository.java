package com.youdeyiwu.repository.user;

import com.youdeyiwu.model.entity.user.UserEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * user.
 *
 * @author dafengzhen
 */
public interface UserRepository extends CrudRepository<UserEntity, Long>,
    PagingAndSortingRepository<UserEntity, Long>, CustomizedUserRepository {

  /**
   * existsByUsername.
   *
   * @param username username
   * @return Boolean
   */
  Boolean existsByUsername(String username);

  /**
   * findByUsername.
   *
   * @param username username
   * @return UserEntity
   */
  UserEntity findByUsername(String username);

}
