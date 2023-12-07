package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * login.
 *
 * @param username username
 * @param password password
 */
public record LoginDto(
    @Length(min = 3, max = 16)
    @NotBlank
    String username,

    @Length(min = 6, max = 18)
    @NotBlank
    String password
) implements Serializable {

}
