package com.youdeyiwu.model.dto.message;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import java.util.Map;

/**
 * create global message.
 *
 * @param name     name
 * @param overview overview
 * @param link     link
 * @param links    links
 * @param content  content
 * @param sort     sort
 */
public record CreateGlobalMessageDto(
    @NotBlank
    String name,

    @NotBlank
    String overview,

    String link,

    Map<String, String> links,

    Map<String, String> content,

    @PositiveOrZero
    Integer sort
) implements Serializable {

}
