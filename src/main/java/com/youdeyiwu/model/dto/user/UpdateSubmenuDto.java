package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import java.util.Set;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.URL;

/**
 * update submenu.
 *
 * @param name    name
 * @param link    link
 * @param sort    sort
 * @param menu    menu
 * @param actions actions
 */
public record UpdateSubmenuDto(
    @Length(min = 1, max = 15, message = "{submenu.name.size}")
    String name,

    @URL(regexp = "^(http://|https://|/).*", message = "{submenu.link.url}")
    String link,

    @PositiveOrZero(message = "{submenu.sort.value}")
    Integer sort,

    Long menu,

    Set<Long> actions
) implements Serializable {

}
