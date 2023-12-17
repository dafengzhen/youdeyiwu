package com.youdeyiwu.model.dto.message;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Map;

/**
 * create message.
 *
 * @param name     name
 * @param overview overview
 * @param content  content
 * @param receiver receiver
 */
public record CreateMessageDto(
    @NotBlank
    String name,

    @NotBlank
    String overview,

    Map<String, String> content,

    @NotNull
    Long receiver
) implements Serializable {

}
