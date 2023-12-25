package com.youdeyiwu.enums.forum;

import lombok.Getter;

/**
 * post sort state.
 * If you need to add an enumeration, please append it at the end.
 * This is because the enumeration is stored in natural order, not by name.
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
  GLOBAL_TOP

}
