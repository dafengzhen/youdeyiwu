package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create action.
 *
 * @param name  name
 * @param alias alias
 * @param sort  sort
 */
public record CreateActionDto(
    @Length(min = 1, max = 50, message = "{action.name.size}")
    @NotBlank(message = "{action.name.required}")
    String name,

    String alias,

    @PositiveOrZero(message = "{action.sort.value}")
    Integer sort
) implements Serializable {

}
