package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * update section group.
 *
 * @param name name
 * @param sort sort
 */
public record UpdateSectionGroupDto(
    String name,

    Integer sort
) implements Serializable {

}
