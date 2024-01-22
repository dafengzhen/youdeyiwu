package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * approved post review queue.
 *
 * @param reason reason
 */
public record ApprovedPostReviewQueueDto(
    String reason
) implements Serializable {

}
