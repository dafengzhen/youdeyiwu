package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * refund post review queue.
 *
 * @param refundReason refundReason
 */
public record RefundPostReviewQueueDto(
    String refundReason
) implements Serializable {

}
