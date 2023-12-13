package com.youdeyiwu.model.dto.user;

import java.io.Serializable;

/**
 * update action.
 *
 * @param name    name
 * @param alias   alias
 * @param sort    sort
 * @param menu    menu
 * @param submenu submenu
 */
public record UpdateActionDto(
    String name,

    String alias,

    Integer sort,

    Long menu,

    Long submenu
) implements Serializable {

}
