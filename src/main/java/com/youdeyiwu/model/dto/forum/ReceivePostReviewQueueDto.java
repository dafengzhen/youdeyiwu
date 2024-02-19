package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotNull;
import java.io.Serializable;
import java.time.LocalDate;

/**
 * receive post review queue.
 *
 * @param latestReviewResultTime latestReviewResultTime
 * @param postId                 postId
 */
public record ReceivePostReviewQueueDto(
    @NotNull(message = "{postReviewQueue.latestReviewResultTime.required}")
    LocalDate latestReviewResultTime,

    @NotNull(message = "{postReviewQueue.postId.required}")
    Long postId
) implements Serializable {

}
