package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * update section group.
 *
 * @param name name
 * @param sort sort
 */
public record UpdateSectionGroupDto(
    @Length(min = 1, max = 50, message = "{sectionGroup.name.size}")
    String name,

    @PositiveOrZero(message = "{sectionGroup.sort.value}")
    Integer sort
) implements Serializable {

}
