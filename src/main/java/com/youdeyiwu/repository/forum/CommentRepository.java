package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * comment.
 *
 * @author dafengzhen
 */
public interface CommentRepository extends JpaRepositoryImplementation<CommentEntity, Long> {

  /**
   * findAllByPost.
   *
   * @param post     post
   * @param pageable pageable
   * @return Page
   */
  Page<CommentEntity> findAllByPost(PostEntity post, Pageable pageable);
}
