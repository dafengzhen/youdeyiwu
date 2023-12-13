package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;

/**
 * create submenu.
 *
 * @param name name
 * @param link link
 * @param sort sort
 */
public record CreateSubmenuDto(
    @NotBlank
    String name,

    @NotBlank
    String link,

    @PositiveOrZero
    Integer sort
) implements Serializable {

}
