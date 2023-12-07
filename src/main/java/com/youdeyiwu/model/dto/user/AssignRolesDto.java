package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

/**
 * assign roles.
 *
 * @param ids ids
 */
public record AssignRolesDto(
    @NotEmpty
    List<Long> ids
) implements Serializable {

}
