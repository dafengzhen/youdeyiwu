package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.enums.forum.CommentReviewStateEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * comment.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class CommentEntityVo extends AbstractEntityVo {

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
   * user.
   */
  private UserEntityVo user;

  /**
   * unique identifier.
   */
  private String uniqueIdentifier;

}