package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.Set;

/**
 * update roles menu.
 *
 * @param roles roles
 */
public record UpdateRolesMenuDto(
    Set<Long> roles
) implements Serializable {

}
