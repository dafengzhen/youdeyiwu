package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * reply.
 *
 * @author dafengzhen
 */
public interface ReplyRepository extends JpaRepositoryImplementation<QuoteReplyEntity, Long> {

}
