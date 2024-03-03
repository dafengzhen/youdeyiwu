package com.youdeyiwu.repository.user;

import com.youdeyiwu.model.entity.user.UserEntity;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * user.
 *
 * @author dafengzhen
 */
public interface UserRepository
    extends JpaRepositoryImplementation<UserEntity, Long>, CustomizedUserRepository {

  /**
   * existsByUsername.
   *
   * @param username username
   * @return boolean
   */
  boolean existsByUsername(String username);

  /**
   * findByUsername.
   *
   * @param username username
   * @return UserEntity
   */
  UserEntity findByUsername(String username);
}
