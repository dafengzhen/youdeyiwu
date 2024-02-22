package com.youdeyiwu.model.dto.user;

import com.youdeyiwu.annotation.UrlIfNotEmpty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create menu.
 *
 * @param name name
 * @param link link
 * @param sort sort
 */
public record CreateMenuDto(
    @Length(min = 1, max = 50, message = "{menu.name.size}")
    @NotBlank(message = "{menu.name.required}")
    String name,

    @UrlIfNotEmpty(message = "{menu.link.url}")
    @NotBlank(message = "{menu.link.required}")
    String link,

    @PositiveOrZero(message = "{menu.sort.value}")
    Integer sort
) implements Serializable {

}
