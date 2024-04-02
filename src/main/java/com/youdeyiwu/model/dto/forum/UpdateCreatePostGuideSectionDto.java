package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * update create post guide.
 *
 * @param createPostGuide createPostGuide
 */
public record UpdateCreatePostGuideSectionDto(
    String createPostGuide
) implements Serializable {

}
