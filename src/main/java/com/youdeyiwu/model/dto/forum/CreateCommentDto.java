package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create comment.
 *
 * @param content content
 * @param postId  postId
 */
public record CreateCommentDto(
    @Length(min = 1, max = 255, message = "{comment.content.size}")
    @NotBlank(message = "{comment.content.required}")
    String content,

    @NotNull(message = "{comment.postId.required}")
    Long postId
) implements Serializable {

}
