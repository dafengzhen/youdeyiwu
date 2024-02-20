package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * return post review queue.
 *
 * @param reason reason
 */
public record ReturnPostReviewQueueDto(
    String reason
) implements Serializable {

}
