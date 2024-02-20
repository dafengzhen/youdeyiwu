package com.youdeyiwu.model.dto.message;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Map;
import org.hibernate.validator.constraints.Length;

/**
 * create message.
 *
 * @param name     name
 * @param overview overview
 * @param link     link
 * @param links    links
 * @param content  content
 * @param receiver receiver
 */
public record CreateMessageDto(
    @Length(min = 1, max = 120, message = "{message.name.size}")
    @NotBlank(message = "{message.name.required}")
    String name,

    @Length(min = 1, max = 512, message = "{message.overview.size}")
    @NotBlank(message = "{message.overview.required}")
    String overview,

    String link,

    Map<String, String> links,

    Map<String, String> content,

    @NotNull(message = "{message.receiver.required}")
    Long receiver
) implements Serializable {

}
