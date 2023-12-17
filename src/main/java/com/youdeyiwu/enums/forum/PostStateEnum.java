package com.youdeyiwu.enums.forum;

import lombok.Getter;

/**
 * post state.
 * If you need to add an enumeration, please append it at the end.
 * This is because the enumeration is stored in natural order, not by name.
 *
 * @author dafengzhen
 */
@Getter
public enum PostStateEnum {

  /**
   * show (Default).
   */
  SHOW,

  /**
   * hide (Forum Administrator, Access to Section Management).
   */
  HIDE,

  /**
   * lock (The forum administrators, section moderators,
   * as well as the thread authors and users with access keys, can access).
   */
  LOCK,

  /**
   * allow (The forum administrators, section moderators,
   * as well as the thread authors and whitelisted users can access).
   */
  ALLOW,

  /**
   * block (Forum administrators, section administrators,
   * and post authors have access, while users on the blacklist cannot access).
   */
  BLOCK,

  /**
   * visible after login.
   */
  VISIBLE_AFTER_LOGIN

}