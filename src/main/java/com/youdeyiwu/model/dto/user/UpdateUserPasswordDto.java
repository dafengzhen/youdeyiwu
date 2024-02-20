package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;

/**
 * update user password.
 *
 * @param oldPassword oldPassword
 * @param newPassword newPassword
 */
public record UpdateUserPasswordDto(
    @Length(min = 6, max = 18, message = "{user.password.old.size}")
    @NotBlank(message = "{user.password.old.required}")
    String oldPassword,

    @Length(min = 6, max = 18, message = "{user.password.size}")
    @NotBlank(message = "{user.password.required}")
    String newPassword
) implements Serializable {

}
