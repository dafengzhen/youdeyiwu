package com.youdeyiwu.model.dto.user;

import com.youdeyiwu.annotation.UrlIfNotEmpty;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import java.util.Set;
import org.hibernate.validator.constraints.Length;

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
    @Length(min = 1, max = 50, message = "{submenu.name.size}")
    String name,

    @UrlIfNotEmpty(message = "{submenu.link.url}")
    String link,

    @PositiveOrZero(message = "{submenu.sort.value}")
    Integer sort,

    Long menu,

    Set<Long> actions
) implements Serializable {

}
