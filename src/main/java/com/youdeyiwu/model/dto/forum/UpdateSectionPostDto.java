package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * update section post.
 *
 * @param sectionId     sectionId
 * @param removeSection removeSection
 */
public record UpdateSectionPostDto(
    Long sectionId,

    Boolean removeSection
) implements Serializable {

}
