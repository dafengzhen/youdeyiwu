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
   * Create a new post.
   */
  CREATE_POST,

  /**
   * Create a new comment.
   */
  CREATE_COMMENT,

  /**
   * Like a post.
   */
  LIKE_POST,

  /**
   * Like a comment.
   */
  LIKE_COMMENT,

  /**
   * Update an existing post.
   */
  UPDATE_POST,

  /**
   * Follow a post.
   */
  FOLLOW_POST,

  /**
   * Favorite a post.
   */
  FAVORITE_POST,

  /**
   * Create a reply to a comment.
   */
  CREATE_REPLY,

  /**
   * Add a tag to a post.
   */
  ADD_POST_TAG,

  /**
   * Add a content link to a post.
   */
  ADD_POST_CONTENT_LINK,

  /**
   * Add a cover link to a post.
   */
  ADD_POST_COVER_LINK,

  /**
   * Add a section to a post.
   */
  ADD_POST_SECTION

}
