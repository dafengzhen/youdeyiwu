package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import lombok.Data;

/**
 * comment reply.
 *
 * @author dafengzhen
 */
@Data
public class CommentReplyEntityVo {

  /**
   * comment.
   */
  private CommentEntity comment;

  /**
   * reply.
   */
  private QuoteReplyEntity reply;

}