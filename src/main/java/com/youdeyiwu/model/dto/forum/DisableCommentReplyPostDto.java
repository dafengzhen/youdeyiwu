package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * disable comment reply.
 *
 * @param disableComments      disableComments
 * @param disableReplies       disableReplies
 * @param commentDisableReason commentDisableReason
 * @param replyDisableReason   replyDisableReason
 */
public record DisableCommentReplyPostDto(
    Boolean disableComments,

    Boolean disableReplies,

    String commentDisableReason,

    String replyDisableReason
) implements Serializable {

}
