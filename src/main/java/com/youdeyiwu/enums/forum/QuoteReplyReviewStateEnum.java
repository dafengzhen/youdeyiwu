package com.youdeyiwu.enums.forum;

import lombok.Getter;

/**
 * quote reply review state.
 * If you need to add an enumeration, please append it at the end.
 * This is because the enumeration is stored in natural order, not by name.
 *
 * @author dafengzhen
 */
@Getter
public enum QuoteReplyReviewStateEnum {

  /**
   * approved (Default).
   */
  APPROVED,

  /**
   * rejected (The forum administrators, section administrators, and comment authors have access).
   */
  REJECTED,

  /**
   * pendingReview (The forum administrators, section administrators, and comment authors have access).
   */
  PENDING_REVIEW,

}