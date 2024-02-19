package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create tag group.
 *
 * @param name name
 */
public record CreateTagGroupDto(
    @Length(min = 1, max = 15, message = "{tagGroup.name.size}")
    @NotBlank(message = "{tagGroup.name.required}")
    String name
) implements Serializable {

}
