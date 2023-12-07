package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.PostFavoriteEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import java.util.Set;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * post favorite.
 *
 * @author dafengzhen
 */
public interface PostFavoriteRepository extends CrudRepository<PostFavoriteEntity, Long>,
    PagingAndSortingRepository<PostFavoriteEntity, Long>, CustomizedPostRepository {

  /**
   * findAllByUser.
   *
   * @param user user
   * @return Set
   */
  Set<PostFavoriteEntity> findAllByUser(UserEntity user);
}
