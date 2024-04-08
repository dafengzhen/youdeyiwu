package com.youdeyiwu.model.dto.user;

import java.io.Serializable;

/**
 * disable comment reply.
 *
 * @param noPostingAllowed     noPostingAllowed
 * @param disableComments      disableComments
 * @param disableReplies       disableReplies
 * @param noPostingReason      noPostingReason
 * @param commentDisableReason commentDisableReason
 * @param replyDisableReason   replyDisableReason
 */
public record DisableCommentReplyUserDto(
    Boolean noPostingAllowed,

    Boolean disableComments,

    Boolean disableReplies,

    String noPostingReason,

    String commentDisableReason,

    String replyDisableReason
) implements Serializable {

}
