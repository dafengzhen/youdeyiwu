package com.youdeyiwu.model.dto.user;

import java.io.Serializable;

/**
 * update user profile.
 *
 * @param alias       alias
 * @param avatar      avatar
 * @param oneSentence oneSentence
 */
public record UpdateUserProfileDto(
    String alias,

    String avatar,

    String oneSentence
) implements Serializable {

}
