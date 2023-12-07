package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * query params.
 *
 * @param sectionGroupId sectionGroupId
 * @param sectionId      sectionId
 * @param tagGroupId     tagGroupId
 * @param tagId          tagId
 */
public record QueryParamsPostDto(
    Long sectionGroupId,

    Long sectionId,

    Long tagGroupId,

    Long tagId
) implements Serializable {

}
