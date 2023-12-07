package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * create section group.
 *
 * @param name name
 * @param sort sort
 */
public record CreateSectionGroupDto(
    @Length(min = 1)
    @NotBlank
    String name,

    Integer sort
) implements Serializable {

}
