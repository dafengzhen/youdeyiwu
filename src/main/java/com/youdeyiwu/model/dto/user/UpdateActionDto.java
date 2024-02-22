package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * update action.
 *
 * @param name    name
 * @param alias   alias
 * @param sort    sort
 * @param menu    menu
 * @param submenu submenu
 */
public record UpdateActionDto(
    @Length(min = 1, max = 50, message = "{action.name.size}")
    String name,

    String alias,

    @PositiveOrZero(message = "{action.sort.value}")
    Integer sort,

    Long menu,

    Long submenu
) implements Serializable {

}
