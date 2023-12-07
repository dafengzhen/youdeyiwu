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
    @Length(min = 1)
    @NotBlank
    String name
) implements Serializable {

}
