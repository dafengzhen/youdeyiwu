package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * register.
 *
 * @param alias    alias
 * @param username username
 * @param password password
 */
public record RegisterDto(
    @Length(min = 3, max = 16)
    String alias,

    @Length(min = 3, max = 16)
    @NotBlank
    String username,

    @Length(min = 6, max = 18)
    @NotBlank
    String password
) implements Serializable {

}
