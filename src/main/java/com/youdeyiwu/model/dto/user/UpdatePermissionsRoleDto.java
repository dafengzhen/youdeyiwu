package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.Set;

/**
 * update permissions.
 *
 * @param permissions permissions
 */
public record UpdatePermissionsRoleDto(
    Set<Long> permissions
) implements Serializable {

}
