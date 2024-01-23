package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import java.io.Serializable;
import lombok.Data;

/**
 * comment reply.
 *
 * @author dafengzhen
 */
@Data
public class CommentReplyEntityVo implements Serializable {

  /**
   * comment.
   */
  private CommentEntity comment;

  /**
   * reply.
   */
  private QuoteReplyEntity reply;

}