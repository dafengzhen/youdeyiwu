package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

/**
 * assign permissions.
 *
 * @param ids ids
 */
public record AssignPermissionsDto(
    @NotEmpty
    List<Long> ids
) implements Serializable {

}
