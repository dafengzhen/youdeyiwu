package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;
import java.util.Set;

/**
 * update tags tag group.
 *
 * @param tags tags
 */
public record UpdateTagsTagGroupDto(
    Set<Long> tags
) implements Serializable {

}
