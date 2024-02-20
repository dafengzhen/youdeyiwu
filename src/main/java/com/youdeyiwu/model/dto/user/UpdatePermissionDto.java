package com.youdeyiwu.model.dto.user;

import com.youdeyiwu.enums.permission.MatcherTypeEnum;
import com.youdeyiwu.enums.permission.MethodTypeEnum;
import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import java.util.Set;
import org.hibernate.validator.constraints.Length;

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
    @Length(min = 1, max = 120, message = "{permission.name.size}")
    String name,

    String alias,

    String overview,

    MethodTypeEnum method,

    MatcherTypeEnum type,

    Boolean caseInsensitive,

    @PositiveOrZero(message = "{permission.sort.value}")
    Integer sort,

    Set<Long> matchers
) implements Serializable {

}
