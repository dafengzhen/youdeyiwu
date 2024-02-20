package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.PostReviewHistoryEntity;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * post review history.
 *
 * @author dafengzhen
 */
public interface PostReviewHistoryRepository
    extends JpaRepositoryImplementation<PostReviewHistoryEntity, Long> {

}
