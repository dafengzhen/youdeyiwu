package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import java.io.Serializable;

/**
 * point permission rule.
 *
 * @param permissionRuleName permissionRuleName
 */
public record PointPermissionRuleEventDto(
    PermissionRuleNameEnum permissionRuleName
) implements Serializable {

}
