package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Set;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.URL;
import org.springframework.web.multipart.MultipartFile;

/**
 * update post.
 *
 * @param name          name
 * @param cover         cover
 * @param overview      overview
 * @param content       content
 * @param contentLink   contentLink
 * @param tags          tags
 * @param sectionId     sectionId
 * @param removeSection removeSection
 */
public record UpdatePostDto(
    @Length(min = 1, max = 120, message = "{post.name.size}")
    @NotBlank(message = "{post.name.required}")
    String name,

    @URL(regexp = "^(http|https).*", message = "{post.cover.url}")
    String cover,

    String overview,

    String content,

    @URL(regexp = "^(http|https).*", message = "{post.content.link}")
    String contentLink,

    Set<String> tags,

    Long sectionId,

    Boolean removeSection
) implements Serializable {

}
