package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;
import java.util.Set;
import org.springframework.web.multipart.MultipartFile;

/**
 * update post.
 *
 * @param name          name
 * @param cover         cover
 * @param coverImage    coverImage
 * @param overview      overview
 * @param content       content
 * @param contentLink   contentLink
 * @param tags          tags
 * @param sectionId     sectionId
 * @param removeSection removeSection
 */
public record UpdatePostDto(
    String name,

    String cover,

    MultipartFile coverImage,

    String overview,

    String content,

    String contentLink,

    Set<String> tags,

    Long sectionId,

    Boolean removeSection
) implements Serializable {

}
