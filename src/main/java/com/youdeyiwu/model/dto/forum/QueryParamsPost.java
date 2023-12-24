package com.youdeyiwu.model.dto.forum;

import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.forum.SectionGroupEntity;
import com.youdeyiwu.model.entity.forum.TagEntity;
import com.youdeyiwu.model.entity.forum.TagGroupEntity;
import java.io.Serializable;

/**
 * query params.
 *
 * @param sectionGroup sectionGroup
 * @param section      section
 * @param tagGroup     tagGroup
 * @param tag          tag
 */
public record QueryParamsPost(
    SectionGroupEntity sectionGroup,

    SectionEntity section,

    TagGroupEntity tagGroup,

    TagEntity tag
) implements Serializable {

}
