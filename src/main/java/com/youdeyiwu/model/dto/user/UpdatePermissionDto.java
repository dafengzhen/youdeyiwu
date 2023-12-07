package com.youdeyiwu.model.dto.user;

import com.youdeyiwu.enums.permission.MatcherTypeEnum;
import com.youdeyiwu.enums.permission.MethodTypeEnum;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import java.util.Set;

/**
 * update permission.
 *
 * @param name            name
 * @param alias           alias
 * @param overview        overview
 * @param method          method
 * @param type            type
 * @param caseInsensitive caseInsensitive
 * @param sort            sort
 * @param matchers        matchers
 */
public record UpdatePermissionDto(
    String name,

    String alias,

    String overview,

    MethodTypeEnum method,

    MatcherTypeEnum type,

    Boolean caseInsensitive,

    @PositiveOrZero
    Integer sort,

    Set<Long> matchers
) implements Serializable {

}
