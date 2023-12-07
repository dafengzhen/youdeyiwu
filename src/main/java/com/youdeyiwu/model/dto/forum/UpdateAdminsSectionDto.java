package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;
import java.util.Set;

/**
 * update admins section.
 *
 * @param admins admins
 */
public record UpdateAdminsSectionDto(
    Set<Long> admins
) implements Serializable {

}
