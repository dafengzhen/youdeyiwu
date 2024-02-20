package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * update role.
 *
 * @param name     name
 * @param overview overview
 * @param sort     sort
 * @param display  display
 */
public record UpdateRoleDto(
    @Length(min = 1, max = 10, message = "{role.name.size}")
    String name,

    String overview,

    @PositiveOrZero(message = "{role.sort.value}")
    Integer sort,

    Boolean display
) implements Serializable {

}
