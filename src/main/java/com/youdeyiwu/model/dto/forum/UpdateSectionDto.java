package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.URL;

/**
 * update section.
 *
 * @param name     name
 * @param cover    cover
 * @param overview overview
 * @param content  content
 * @param sort     sort
 */
public record UpdateSectionDto(
    @Length(min = 1, max = 15, message = "{section.name.size}")
    String name,

    @URL(regexp = "^(http://|https://|/).*", message = "{section.cover.url}")
    String cover,

    String overview,

    String content,

    @PositiveOrZero(message = "{section.sort.value}")
    Integer sort
) implements Serializable {

}
