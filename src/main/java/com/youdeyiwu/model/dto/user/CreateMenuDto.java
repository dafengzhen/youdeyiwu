package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.URL;

/**
 * create menu.
 *
 * @param name name
 * @param link link
 * @param sort sort
 */
public record CreateMenuDto(
    @Length(min = 1, max = 15, message = "{menu.name.size}")
    @NotBlank(message = "{menu.name.required}")
    String name,

    @URL(regexp = "^(http://|https://|/).*", message = "{menu.link.url}")
    @NotBlank(message = "{menu.link.required}")
    String link,

    @PositiveOrZero(message = "{menu.sort.value}")
    Integer sort
) implements Serializable {

}
