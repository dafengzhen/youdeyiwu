package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * update tag.
 *
 * @param name name
 * @param sort sort
 */
public record UpdateTagDto(
    String name,

    Integer sort
) implements Serializable {

}
