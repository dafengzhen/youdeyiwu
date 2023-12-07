package com.youdeyiwu.model.dto.user;

import java.io.Serializable;

/**
 * update user states.
 *
 * @param accountNonExpired     accountNonExpired
 * @param credentialsNonExpired credentialsNonExpired
 * @param accountNonLocked      accountNonLocked
 * @param enabled               enabled
 */
public record UpdateUserStatesDto(
    Boolean accountNonExpired,

    Boolean credentialsNonExpired,

    Boolean accountNonLocked,

    Boolean enabled
) implements Serializable {

}
