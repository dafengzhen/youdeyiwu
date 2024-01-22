package com.youdeyiwu.model.dto.forum;

import com.youdeyiwu.enums.forum.PostReviewStateEnum;
import com.youdeyiwu.enums.forum.PostSortStateEnum;
import com.youdeyiwu.enums.forum.PostStateEnum;
import java.io.Serializable;
import java.util.Set;

/**
 * update states post.
 *
 * @param states       states
 * @param reviewState  reviewState
 * @param sortState    sortState
 * @param allows       allows
 * @param blocks       blocks
 * @param accessKey    accessKey
 * @param reason       reason
 * @param reviewReason reviewReason
 */
public record UpdateStatesPostDto(
    Set<PostStateEnum> states,

    PostReviewStateEnum reviewState,

    PostSortStateEnum sortState,

    Set<Long> allows,

    Set<Long> blocks,

    String accessKey,

    String reason,

    String reviewReason
) implements Serializable {

}
