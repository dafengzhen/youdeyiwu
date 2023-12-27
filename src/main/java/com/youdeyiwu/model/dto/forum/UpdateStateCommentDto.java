package com.youdeyiwu.model.dto.forum;

import com.youdeyiwu.enums.forum.CommentReviewStateEnum;
import java.io.Serializable;

/**
 * update state comment.
 *
 * @param reviewState reviewState
 * @param reason      reason
 */
public record UpdateStateCommentDto(
    CommentReviewStateEnum reviewState,

    String reason
) implements Serializable {

}
