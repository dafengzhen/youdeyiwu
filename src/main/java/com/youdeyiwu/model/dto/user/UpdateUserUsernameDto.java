package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * update user username.
 *
 * @param username username
 */
public record UpdateUserUsernameDto(
    @Length(min = 3, max = 16)
    @NotBlank
    String username
) implements Serializable {

}
