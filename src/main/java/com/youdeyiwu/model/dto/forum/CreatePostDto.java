package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Set;
import org.hibernate.validator.constraints.Length;

/**
 * create post.
 *
 * @param name        name
 * @param cover       cover
 * @param overview    overview
 * @param content     content
 * @param contentLink contentLink
 * @param tags        tags
 * @param sectionId   sectionId
 */
public record CreatePostDto(
    @Length(min = 1)
    @NotBlank
    String name,

    String cover,

    String overview,

    String content,

    String contentLink,

    Set<String> tags,

    String sectionId
) implements Serializable {

}
