package com.youdeyiwu.enums.message;

import lombok.Getter;

/**
 * message type.
 *
 * @author dafengzhen
 */
@Getter
public enum MessageTypeEnum {

  /**
   * system.
   */
  SYSTEM,

  /**
   * user.
   */
  USER,

  /**
   * role.
   */
  ROLE,

  /**
   * permission.
   */
  PERMISSION,

  /**
   * section.
   */
  SECTION,

  /**
   * section group.
   */
  SECTION_GROUP,

  /**
   * post.
   */
  POST,

  /**
   * tag.
   */
  TAG,

  /**
   * tag group.
   */
  TAG_GROUP,

  /**
   * comment.
   */
  COMMENT,

  /**
   * reply.
   */
  REPLY,

}