package com.youdeyiwu.model.dto.forum;

import com.youdeyiwu.enums.forum.QuoteReplyReviewStateEnum;
import java.io.Serializable;

/**
 * update state reply.
 *
 * @param reviewState reviewState
 * @param reason      reason
 */
public record UpdateStateReplyDto(
    QuoteReplyReviewStateEnum reviewState,

    String reason
) implements Serializable {

}
