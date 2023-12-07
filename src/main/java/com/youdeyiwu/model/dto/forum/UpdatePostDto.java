package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;
import java.util.Set;

/**
 * update post.
 *
 * @param name        name
 * @param cover       cover
 * @param overview    overview
 * @param content     content
 * @param contentLink contentLink
 * @param tags        tags
 * @param sectionId   sectionId
 */
public record UpdatePostDto(
    String name,

    String cover,

    String overview,

    String content,

    String contentLink,

    Set<String> tags,

    String sectionId
) implements Serializable {

}
