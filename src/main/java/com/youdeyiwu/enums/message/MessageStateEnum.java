package com.youdeyiwu.enums.message;

import lombok.Getter;

/**
 * message state.
 * If you need to add an enumeration, please append it at the end.
 * This is because the enumeration is stored in natural order, not by name.
 *
 * @author dafengzhen
 */
@Getter
public enum MessageStateEnum {

  /**
   * unread.
   */
  UNREAD,

  /**
   * read.
   */
  READ

}
