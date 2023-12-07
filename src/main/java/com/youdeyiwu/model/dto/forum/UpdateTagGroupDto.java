package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * update tag group.
 *
 * @param name name
 * @param sort sort
 */
public record UpdateTagGroupDto(
    String name,

    Integer sort
) implements Serializable {

}
