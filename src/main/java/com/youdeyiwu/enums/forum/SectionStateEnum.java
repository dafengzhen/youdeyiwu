package com.youdeyiwu.enums.forum;

import lombok.Getter;

/**
 * section state.
 *
 * @author dafengzhen
 */
@Getter
public enum SectionStateEnum {

  /**
   * show (Default).
   */
  SHOW,

  /**
   * hide (Forum Administrator, Access to Section Management).
   */
  HIDE,

  /**
   * lock (The forum administrator, section manager, and users with access keys can access).
   */
  LOCK,

  /**
   * allow (The forum administrator, section administrator, and whitelisted users have access).
   */
  ALLOW,

  /**
   * block (The forum administrators and section moderators have access,
   * while users on the blacklist are not allowed to access).
   */
  BLOCK,

  /**
   * visible after login.
   */
  VISIBLE_AFTER_LOGIN

}