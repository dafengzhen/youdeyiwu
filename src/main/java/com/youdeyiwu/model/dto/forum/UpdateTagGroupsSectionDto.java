package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;
import java.util.Set;

/**
 * update tag groups section.
 *
 * @param tagGroups tagGroups
 */
public record UpdateTagGroupsSectionDto(
    Set<Long> tagGroups
) implements Serializable {

}
