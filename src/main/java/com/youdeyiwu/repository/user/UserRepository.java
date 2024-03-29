package com.youdeyiwu.repository.user;

import com.youdeyiwu.model.entity.user.UserEntity;
import java.util.Optional;
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
   * @return Optional
   */
  Optional<UserEntity> findByUsername(String username);
}
