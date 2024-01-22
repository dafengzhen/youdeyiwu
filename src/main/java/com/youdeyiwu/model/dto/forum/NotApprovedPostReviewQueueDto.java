package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * not approved post review queue.
 *
 * @param reason reason
 */
public record NotApprovedPostReviewQueueDto(
    String reason
) implements Serializable {

}
