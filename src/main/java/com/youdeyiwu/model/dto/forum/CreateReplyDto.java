package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create reply.
 *
 * @param content   content
 * @param commentId commentId
 * @param replyId   replyId
 */
public record CreateReplyDto(
    @Length(min = 1)
    @NotBlank
    String content,

    Long commentId,

    Long replyId
) implements Serializable {

}
