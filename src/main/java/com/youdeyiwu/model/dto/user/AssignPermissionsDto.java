package com.youdeyiwu.model.dto.user;

import java.io.Serializable;
import java.util.List;

/**
 * assign permissions.
 *
 * @param ids ids
 */
public record AssignPermissionsDto(
    List<Long> ids
) implements Serializable {

}
