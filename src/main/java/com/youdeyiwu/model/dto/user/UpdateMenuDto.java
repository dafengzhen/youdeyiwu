package com.youdeyiwu.model.dto.user;

import java.io.Serializable;

/**
 * update menu.
 *
 * @param name name
 * @param link link
 * @param sort sort
 */
public record UpdateMenuDto(
    String name,

    String link,

    Integer sort
) implements Serializable {

}
