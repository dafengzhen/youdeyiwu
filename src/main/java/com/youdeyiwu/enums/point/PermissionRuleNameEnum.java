package com.youdeyiwu.enums.point;

import lombok.Getter;

/**
 * permission rule name.
 *
 * @author dafengzhen
 */
@Getter
public enum PermissionRuleNameEnum {

  /**
   * permission to create a new post.
   */
  CREATE_POST,

  /**
   * permission to create a comment.
   */
  CREATE_COMMENT,

  /**
   * permission to create a reply.
   */
  CREATE_REPLY,

  /**
   * permission to update a post.
   */
  UPDATE_POST,

  /**
   * permission to add tags to a post.
   */
  ADD_POST_TAG,

  /**
   * permission to add content links to a post.
   */
  ADD_POST_CONTENT_LINK,

  /**
   * permission to add cover links to a post.
   */
  ADD_POST_COVER_LINK,

  /**
   * permission to add sections to a post.
   */
  ADD_POST_SECTION

}
