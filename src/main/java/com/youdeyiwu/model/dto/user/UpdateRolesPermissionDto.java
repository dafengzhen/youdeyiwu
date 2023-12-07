package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.Set;

/**
 * update roles.
 *
 * @param roles roles
 */
public record UpdateRolesPermissionDto(
    Set<Long> roles
) implements Serializable {

}
