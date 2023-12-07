package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;
import java.util.Set;

/**
 * update sections section group.
 *
 * @param sections sections
 */
public record UpdateSectionsSectionGroupDto(
    Set<Long> sections
) implements Serializable {

}
