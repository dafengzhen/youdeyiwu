package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.Set;

/**
 * update roles.
 *
 * @param roles roles
 */
public record UpdateRolesActionDto(
    Set<Long> roles
) implements Serializable {

}
