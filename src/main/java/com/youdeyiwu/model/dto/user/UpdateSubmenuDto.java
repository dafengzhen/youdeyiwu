package com.youdeyiwu.model.dto.user;

import java.io.Serializable;

/**
 * update submenu.
 *
 * @param name name
 * @param link link
 * @param sort sort
 */
public record UpdateSubmenuDto(
    String name,

    String link,

    Integer sort
) implements Serializable {

}
