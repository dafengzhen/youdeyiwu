package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.dto.forum.UpdateStateReplyDto;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.vo.forum.QuoteReplyEntityVo;

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

  /**
   * update state reply.
   *
   * @param id  id
   * @param dto dto
   */
  void updateState(Long id, UpdateStateReplyDto dto);

  /**
   * query.
   *
   * @param id id
   * @return QuoteReplyEntityVo
   */
  QuoteReplyEntityVo query(Long id);
}