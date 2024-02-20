package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.List;

/**
 * assign roles.
 *
 * @param ids ids
 */
public record AssignRolesDto(
    List<Long> ids
) implements Serializable {

}
