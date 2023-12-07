package com.youdeyiwu.model.vo.forum;

import lombok.Data;

/**
 * comment reply.
 *
 * @author dafengzhen
 */
@Data
public class CommentReplyVo {

  /**
   * comment.
   */
  private CommentEntityVo comment;

  /**
   * reply.
   */
  private QuoteReplyEntityVo reply;

}