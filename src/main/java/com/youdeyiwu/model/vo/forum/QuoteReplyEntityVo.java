package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.enums.forum.CommentReviewStateEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * quote reply.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class QuoteReplyEntityVo extends AbstractEntityVo {

  /**
   * content.
   */
  private String content;

  /**
   * likes count.
   */
  private Long likesCount;

  /**
   * reviewState.
   */
  private CommentReviewStateEnum reviewState;

  /**
   * comment.
   */
  private CommentEntityVo comment;

  /**
   * quote reply.
   */
  private QuoteReplyEntityVo quoteReply;

  /**
   * user.
   */
  private UserEntityVo user;

  /**
   * unique identifier.
   */
  private String uniqueIdentifier;

}