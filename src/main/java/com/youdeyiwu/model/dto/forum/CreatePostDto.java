package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Set;
import org.hibernate.validator.constraints.Length;
import org.springframework.web.multipart.MultipartFile;

/**
 * create post.
 *
 * @param name        name
 * @param cover       cover
 * @param coverImage  coverImage
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

    MultipartFile coverImage,

    String overview,

    String content,

    String contentLink,

    Set<String> tags,

    Long sectionId
) implements Serializable {

}
