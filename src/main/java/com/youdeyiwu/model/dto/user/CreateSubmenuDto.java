package com.youdeyiwu.model.dto.user;

import com.youdeyiwu.annotation.UrlIfNotEmpty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create submenu.
 *
 * @param name name
 * @param link link
 * @param sort sort
 */
public record CreateSubmenuDto(
    @Length(min = 1, max = 50, message = "{submenu.name.size}")
    @NotBlank(message = "{submenu.name.required}")
    String name,

    @UrlIfNotEmpty(message = "{submenu.link.url}")
    @NotBlank(message = "{submenu.link.required}")
    String link,

    @PositiveOrZero(message = "{submenu.sort.value}")
    Integer sort
) implements Serializable {

}
