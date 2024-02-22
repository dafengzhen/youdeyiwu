package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * update tag.
 *
 * @param name name
 * @param sort sort
 */
public record UpdateTagDto(
    @Length(min = 1, max = 50, message = "{tag.name.size}")
    String name,

    @PositiveOrZero(message = "{tag.sort.value}")
    Integer sort
) implements Serializable {

}
