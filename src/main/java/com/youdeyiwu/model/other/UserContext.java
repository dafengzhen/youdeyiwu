package com.youdeyiwu.model.other;

import com.youdeyiwu.model.entity.user.UserEntity;

/**
 * user context.
 *
 * @param anonymous anonymous
 * @param user      user
 * @param root      root
 */
public record UserContext(
    boolean anonymous,

    UserEntity user,

    UserEntity root
) {
}
