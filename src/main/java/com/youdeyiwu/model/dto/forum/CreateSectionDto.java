package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create section.
 *
 * @param name name
 */
public record CreateSectionDto(
    @Length(min = 1, max = 15, message = "{section.name.size}")
    @NotBlank(message = "{section.name.required}")
    String name
) implements Serializable {

}
