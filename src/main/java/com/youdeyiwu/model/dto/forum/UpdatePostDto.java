package com.youdeyiwu.model.dto.forum;

import com.youdeyiwu.annotation.UrlIfNotEmpty;
import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Set;
import org.hibernate.validator.constraints.Length;

/**
 * update post.
 *
 * @param name             name
 * @param cover            cover
 * @param overview         overview
 * @param content          content
 * @param plainTextContent plainTextContent
 * @param markdownContent  markdownContent
 * @param deltaContent     deltaContent
 * @param contentLink      contentLink
 * @param tags             tags
 * @param sectionId        sectionId
 * @param removeSection    removeSection
 */
public record UpdatePostDto(
    @Length(min = 1, max = 120, message = "{post.name.size}")
    @NotBlank(message = "{post.name.required}")
    String name,

    @UrlIfNotEmpty(message = "{post.cover.url}")
    String cover,

    String overview,

    String content,

    String plainTextContent,

    String markdownContent,

    String deltaContent,

    @UrlIfNotEmpty(message = "{post.contentLink.url}")
    String contentLink,

    Set<String> tags,

    Long sectionId,

    Boolean removeSection
) implements Serializable {

}
