package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;
import java.util.Set;

/**
 * update tags section.
 *
 * @param tags tags
 */
public record UpdateTagsSectionDto(
    Set<Long> tags
) implements Serializable {

}
