package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import java.util.Set;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.URL;

/**
 * update menu.
 *
 * @param name     name
 * @param link     link
 * @param sort     sort
 * @param submenus submenus
 * @param actions  actions
 */
public record UpdateMenuDto(
    @Length(min = 1, max = 15, message = "{menu.name.size}")
    String name,

    @URL(regexp = "^(http://|https://|/).*", message = "{menu.link.url}")
    String link,

    @PositiveOrZero(message = "{menu.sort.value}")
    Integer sort,

    Set<Long> submenus,

    Set<Long> actions
) implements Serializable {

}
