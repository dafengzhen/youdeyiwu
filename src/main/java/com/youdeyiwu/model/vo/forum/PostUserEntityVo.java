package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.vo.IdLessAbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import java.util.Objects;
import lombok.Data;

/**
 * post user.
 *
 * @author dafengzhen
 */
@Data
public class PostUserEntityVo extends IdLessAbstractEntityVo {

  /**
   * liked.
   */
  private Boolean liked;

  /**
   * followed.
   */
  private Boolean followed;

  /**
   * favorited.
   */
  private Boolean favorited;

  /**
   * disable comments.
   */
  private Boolean disableComments;

  /**
   * disable replies.
   */
  private Boolean disableReplies;

  /**
   * comment disable reason.
   */
  private String commentDisableReason;

  /**
   * reply disable reason.
   */
  private String replyDisableReason;

  /**
   * post.
   */
  private PostEntityVo post;

  /**
   * user.
   */
  private UserEntityVo user;

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PostUserEntityVo that = (PostUserEntityVo) o;
    return Objects.equals(post, that.post) && Objects.equals(user, that.user);
  }

  @Override
  public int hashCode() {
    return Objects.hash(post, user);
  }
}