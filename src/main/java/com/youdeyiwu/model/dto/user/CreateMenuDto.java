package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;

/**
 * create menu.
 *
 * @param name name
 * @param link link
 * @param sort sort
 */
public record CreateMenuDto(
    @NotBlank
    String name,

    @NotBlank
    String link,

    @PositiveOrZero
    Integer sort
) implements Serializable {

}
