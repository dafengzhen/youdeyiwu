package com.youdeyiwu.model.dto.user;

import java.io.Serializable;

/**
 * update submenu.
 *
 * @param name name
 * @param link link
 * @param sort sort
 * @param menu menu
 */
public record UpdateSubmenuDto(
    String name,

    String link,

    Integer sort,

    Long menu
) implements Serializable {

}
