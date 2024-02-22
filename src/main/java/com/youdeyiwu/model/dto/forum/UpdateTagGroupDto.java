package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * update tag group.
 *
 * @param name name
 * @param sort sort
 */
public record UpdateTagGroupDto(
    @Length(min = 1, max = 50, message = "{tagGroup.name.size}")
    String name,

    @PositiveOrZero(message = "{tagGroup.sort.value}")
    Integer sort
) implements Serializable {

}
