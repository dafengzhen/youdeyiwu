package com.youdeyiwu.enums.forum;

import lombok.Getter;

/**
 * quote reply review state.
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