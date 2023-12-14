package com.youdeyiwu.model.dto.user;

import java.io.Serializable;

/**
 * update role action.
 *
 * @param role role
 */
public record UpdateRoleActionDto(
    Long role
) implements Serializable {

}
