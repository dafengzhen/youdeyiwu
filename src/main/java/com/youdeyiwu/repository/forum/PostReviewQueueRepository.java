package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostReviewQueueEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * post review queue.
 *
 * @author dafengzhen
 */
public interface PostReviewQueueRepository extends CrudRepository<PostReviewQueueEntity, Long>,
    PagingAndSortingRepository<PostReviewQueueEntity, Long> {

  /**
   * existsByPost.
   *
   * @param post post
   * @return boolean
   */
  boolean existsByPost(PostEntity post);

  /**
   * findByPost.
   *
   * @param post post
   * @return PostReviewQueueEntity
   */
  PostReviewQueueEntity findByPost(PostEntity post);
}
