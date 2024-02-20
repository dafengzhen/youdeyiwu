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
    @Length(min = 3, max = 16, message = "{user.alias.size}")
    String alias,

    @Length(min = 3, max = 16, message = "{user.username.size}")
    @NotBlank(message = "{user.username.required}")
    String username,

    @Length(min = 6, max = 18, message = "{user.password.size}")
    @NotBlank(message = "{user.password.required}")
    String password
) implements Serializable {

}
