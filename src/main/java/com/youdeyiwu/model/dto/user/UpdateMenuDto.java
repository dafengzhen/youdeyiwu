package com.youdeyiwu.model.dto.user;

import com.youdeyiwu.annotation.UrlIfNotEmpty;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import java.util.Set;
import org.hibernate.validator.constraints.Length;

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
    @Length(min = 1, max = 50, message = "{menu.name.size}")
    String name,

    @UrlIfNotEmpty(message = "{menu.link.url}")
    String link,

    @PositiveOrZero(message = "{menu.sort.value}")
    Integer sort,

    Set<Long> submenus,

    Set<Long> actions
) implements Serializable {

}
