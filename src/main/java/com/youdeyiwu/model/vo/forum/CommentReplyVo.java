package com.youdeyiwu.model.vo.forum;

import java.io.Serializable;
import lombok.Data;

/**
 * comment reply.
 *
 * @author dafengzhen
 */
@Data
public class CommentReplyVo implements Serializable {

  /**
   * comment.
   */
  private CommentEntityVo comment;

  /**
   * reply.
   */
  private QuoteReplyEntityVo reply;

}