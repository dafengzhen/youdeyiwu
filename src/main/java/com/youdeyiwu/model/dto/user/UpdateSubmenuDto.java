package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.Set;

/**
 * update submenu.
 *
 * @param name    name
 * @param link    link
 * @param sort    sort
 * @param menu    menu
 * @param actions actions
 */
public record UpdateSubmenuDto(
    String name,

    String link,

    Integer sort,

    Long menu,

    Set<Long> actions
) implements Serializable {

}
