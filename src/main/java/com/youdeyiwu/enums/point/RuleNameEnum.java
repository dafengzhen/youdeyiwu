package com.youdeyiwu.enums.point;

import lombok.Getter;

/**
 * rule name.
 *
 * @author dafengzhen
 */
@Getter
public enum RuleNameEnum {

  /**
   * earning points for liking a post.
   */
  LIKE_POST,

  /**
   * earning points for liking a comment.
   */
  LIKE_COMMENT,

  /**
   * earning points for liking a reply.
   */
  LIKE_REPLY,

  /**
   * earning points for commenting on a post.
   */
  COMMENT_POST,

  /**
   * earning points for replying to a post.
   */
  REPLY_POST,

  /**
   * earning points for following a post.
   */
  FOLLOW_POST,

  /**
   * earning points for marking a post as a favorite.
   */
  FAVORITE_POST,

  /**
   * earning points for disliking a post.
   */
  DISLIKE_POST,

  /**
   * earning points for disliking a comment.
   */
  DISLIKE_COMMENT,

  /**
   * earning points for disliking a reply.
   */
  DISLIKE_REPLY,

  /**
   * earning points for having a post approved.
   */
  POST_APPROVED,

  /**
   * earning points for having a post not approved.
   */
  POST_NOT_APPROVED,

  /**
   * earning points for a post pending review.
   */
  POST_PENDING_REVIEW,

  /**
   * earning points for visiting a post.
   */
  VISIT_POST,

  /**
   * earning points for creating a new post.
   */
  CREATE_POST

}
