package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create section group.
 *
 * @param name name
 */
public record CreateSectionGroupDto(
    @Length(min = 1, max = 15, message = "{sectionGroup.name.size}")
    @NotBlank(message = "{sectionGroup.name.required}")
    String name
) implements Serializable {

}
