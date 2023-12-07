package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.Set;

/**
 * update roles user.
 *
 * @param roles roles
 */
public record UpdateRolesUserDto(
    Set<Long> roles
) implements Serializable {

}
