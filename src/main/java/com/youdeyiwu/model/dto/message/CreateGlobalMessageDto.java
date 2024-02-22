package com.youdeyiwu.model.dto.message;

import com.youdeyiwu.annotation.UrlIfNotEmpty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import java.util.Map;
import org.hibernate.validator.constraints.Length;

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
    @Length(min = 1, max = 120, message = "{message.name.size}")
    @NotBlank(message = "{message.name.required}")
    String name,

    @Length(min = 1, max = 512, message = "{message.overview.size}")
    @NotBlank(message = "{message.overview.required}")
    String overview,

    @UrlIfNotEmpty(message = "{message.link.url}")
    String link,

    Map<String, @UrlIfNotEmpty(message = "{message.link.url}") String> links,

    Map<String, String> content,

    @PositiveOrZero(message = "{message.sort.value}")
    Integer sort
) implements Serializable {

}
