package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create tag.
 *
 * @param name name
 */
public record CreateTagDto(
    @Length(min = 1, max = 50, message = "{tag.name.size}")
    @NotBlank(message = "{tag.name.required}")
    String name
) implements Serializable {

}
