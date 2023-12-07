package com.youdeyiwu.enums.forum;

import lombok.Getter;

/**
 * post review state.
 *
 * @author dafengzhen
 */
@Getter
public enum PostReviewStateEnum {

  /**
   * approved (Default).
   */
  APPROVED,

  /**
   * rejected (The forum administrators, section administrators, and post authors have access).
   */
  REJECTED,

  /**
   * pendingReview (The forum administrators, section administrators, and post authors have access).
   */
  PENDING_REVIEW,

}