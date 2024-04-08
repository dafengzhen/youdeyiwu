package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * disable user comment reply.
 *
 * @param disableComments      disableComments
 * @param disableReplies       disableReplies
 * @param commentDisableReason commentDisableReason
 * @param replyDisableReason   replyDisableReason
 */
public record DisableUserCommentReplyPostDto(
    Boolean disableComments,

    Boolean disableReplies,

    String commentDisableReason,

    String replyDisableReason
) implements Serializable {

}
