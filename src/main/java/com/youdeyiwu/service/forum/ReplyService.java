package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;

/**
 * reply.
 *
 * @author dafengzhen
 */
public interface ReplyService {

  /**
   * create.
   *
   * @param dto dto
   * @return QuoteReplyEntity
   */
  QuoteReplyEntity create(CreateReplyDto dto);
}