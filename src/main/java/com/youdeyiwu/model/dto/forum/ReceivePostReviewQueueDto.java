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
    @NotNull
    LocalDate latestReviewResultTime,

    @NotNull
    Long postId
) implements Serializable {

}
