package com.youdeyiwu.enums.point;

import lombok.Getter;

/**
 * auto rule name.
 *
 * @author dafengzhen
 */
@Getter
public enum AutoRuleNameEnum {

  /**
   * Someone liked your post.
   */
  LIKED_YOUR_POST,

  /**
   * Someone liked your comment.
   */
  LIKED_YOUR_COMMENT,

  /**
   * Someone liked your reply.
   */
  LIKED_YOUR_REPLY,

  /**
   * Someone commented on your post.
   */
  COMMENTED_ON_YOUR_POST,

  /**
   * Someone replied to your post.
   */
  REPLIED_TO_YOUR_POST,

  /**
   * Someone followed your post.
   */
  FOLLOWED_YOUR_POST,

  /**
   * Someone bookmarked your post.
   */
  BOOKMARKED_YOUR_POST,

  /**
   * Someone appreciated your post.
   */
  APPRECIATED_YOUR_POST,

  /**
   * Someone disliked your post.
   */
  DISLIKED_YOUR_POST,

  /**
   * Someone disliked your comment.
   */
  DISLIKED_YOUR_COMMENT,

  /**
   * Someone disliked your reply.
   */
  DISLIKED_YOUR_REPLY,

  /**
   * Your post did not pass review.
   */
  POST_NOT_APPROVED,

  /**
   * Your post is awaiting review.
   */
  POST_UNDER_REVIEW,

  /**
   * Someone visited your post.
   */
  VISITED_YOUR_POST

}
