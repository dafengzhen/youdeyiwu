package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.PermissionRuleNameEnum;
import java.io.Serializable;

/**
 * point permission rule.
 *
 * @param permissionRuleName permissionRuleName
 * @param from               from
 * @param link               link
 */
public record PointPermissionRuleEventDto(
    PermissionRuleNameEnum permissionRuleName,

    String from,

    String link
) implements Serializable {

}
