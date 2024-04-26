package com.youdeyiwu.model.dto.forum;

import com.youdeyiwu.annotation.UrlIfNotEmpty;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

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
    @Length(min = 1, max = 50, message = "{section.name.size}")
    String name,

    @UrlIfNotEmpty(message = "{section.cover.url}")
    String cover,

    String overview,

    String content,

    @PositiveOrZero(message = "{section.sort.value}")
    Integer sort,

    @PositiveOrZero(message = "{section.accessPoints.value}")
    Integer accessPoints
) implements Serializable {

}
