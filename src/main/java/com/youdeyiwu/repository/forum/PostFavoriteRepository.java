package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostFavoriteEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import java.util.Optional;
import java.util.Set;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * post favorite.
 *
 * @author dafengzhen
 */
public interface PostFavoriteRepository
    extends JpaRepositoryImplementation<PostFavoriteEntity, Long>, CustomizedPostRepository {

  /**
   * findAllByUser.
   *
   * @param user user
   * @return Set
   */
  Set<PostFavoriteEntity> findAllByUser(UserEntity user);

  /**
   * findByUserAndPost.
   *
   * @param user user
   * @param post post
   * @return Optional
   */
  Optional<PostFavoriteEntity> findByUserAndPost(UserEntity user, PostEntity post);
}
