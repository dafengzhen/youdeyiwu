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
    @Length(min = 6, max = 18)
    @NotBlank
    String oldPassword,

    @Length(min = 6, max = 18)
    @NotBlank
    String newPassword
) implements Serializable {

}
