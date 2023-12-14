package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.Set;

/**
 * update roles submenu.
 *
 * @param roles roles
 */
public record UpdateRolesSubmenuDto(
    Set<Long> roles
) implements Serializable {

}
