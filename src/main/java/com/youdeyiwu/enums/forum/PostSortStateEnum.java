package com.youdeyiwu.enums.forum;

import lombok.Getter;

/**
 * post sort state.
 *
 * @author dafengzhen
 */
@Getter
public enum PostSortStateEnum {

  /**
   * default.
   */
  DEFAULT,

  /**
   * popular.
   */
  POPULAR,

  /**
   * current top (The thread is pinned in the current section).
   */
  CURRENT_TOP,

  /**
   * global top (The thread is pinned across all sections).
   */
  GLOBAL_TOP;

}
