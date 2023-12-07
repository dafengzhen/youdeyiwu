package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * update section.
 *
 * @param name name
 */
public record UpdateSectionDto(
    @Length(min = 1)
    String name,

    // @URL(regexp = "^(http|https).*")
    String cover,

    String overview,

    String content,

    @PositiveOrZero
    Integer sort
) implements Serializable {

}
