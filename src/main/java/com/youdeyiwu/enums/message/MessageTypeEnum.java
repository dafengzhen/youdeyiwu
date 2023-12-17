package com.youdeyiwu.enums.message;

import lombok.Getter;

/**
 * message type.
 * If you need to add an enumeration, please append it at the end.
 * This is because the enumeration is stored in natural order, not by name.
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
   * global message.
   */
  GLOBAL_MESSAGE,

  /**
   * message.
   */
  MESSAGE,

  /**
   * config.
   */
  CONFIG,

  /**
   * file.
   */
  FILE,

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
   * menu.
   */
  MENU,

  /**
   * submenu.
   */
  SUBMENU,

  /**
   * action.
   */
  ACTION,

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

  /**
   * point.
   */
  POINT

}