package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import java.util.Set;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * post.
 *
 * @author dafengzhen
 */
public interface PostRepository extends JpaRepositoryImplementation<PostEntity, Long>, CustomizedPostRepository {

  /**
   * findAllByUser.
   *
   * @param user user
   * @return Set
   */
  Set<PostEntity> findAllByUser(UserEntity user);
}
