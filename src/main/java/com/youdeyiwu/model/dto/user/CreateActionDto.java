package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;

/**
 * create action.
 *
 * @param name  name
 * @param alias alias
 * @param sort  sort
 */
public record CreateActionDto(
    @NotBlank
    String name,

    String alias,

    @PositiveOrZero
    Integer sort
) implements Serializable {

}
