package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.Set;

/**
 * update menu.
 *
 * @param name     name
 * @param link     link
 * @param sort     sort
 * @param submenus submenus
 * @param actions  actions
 */
public record UpdateMenuDto(
    String name,

    String link,

    Integer sort,

    Set<Long> submenus,

    Set<Long> actions
) implements Serializable {

}
