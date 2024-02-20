package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.URL;

/**
 * create submenu.
 *
 * @param name name
 * @param link link
 * @param sort sort
 */
public record CreateSubmenuDto(
    @Length(min = 1, max = 15, message = "{submenu.name.size}")
    @NotBlank(message = "{submenu.name.required}")
    String name,

    @URL(regexp = "^(http://|https://|/).*", message = "{submenu.link.url}")
    @NotBlank(message = "{submenu.link.required}")
    String link,

    @PositiveOrZero(message = "{submenu.sort.value}")
    Integer sort
) implements Serializable {

}
