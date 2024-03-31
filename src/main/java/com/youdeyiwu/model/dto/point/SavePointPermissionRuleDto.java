package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * save point permission rule.
 *
 * @param permissionRuleName permissionRuleName
 * @param requiredPoints     requiredPoints
 * @param enable             enable
 */
public record SavePointPermissionRuleDto(
    @NotNull(message = "{point.permissionRuleName.required}")
    PermissionRuleNameEnum permissionRuleName,

    Integer requiredPoints,

    Integer operationCost,

    Boolean enable
) implements Serializable {

}
