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
    @Length(min = 1)
    @NotBlank
    String content,

    @NotNull
    Long postId
) implements Serializable {

}
