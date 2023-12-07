package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * reply.
 *
 * @author dafengzhen
 */
public interface ReplyRepository extends CrudRepository<QuoteReplyEntity, Long>,
    PagingAndSortingRepository<QuoteReplyEntity, Long> {

}
