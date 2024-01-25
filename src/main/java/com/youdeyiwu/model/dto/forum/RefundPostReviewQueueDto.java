package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * refund post review queue.
 *
 * @param reason reason
 */
public record RefundPostReviewQueueDto(
    String reason
) implements Serializable {

}
