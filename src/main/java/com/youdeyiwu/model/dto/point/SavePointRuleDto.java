package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.RuleNameEnum;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * save point rule.
 *
 * @param ruleName       ruleName
 * @param requiredPoints requiredPoints
 */
public record SavePointRuleDto(
    @NotNull
    RuleNameEnum ruleName,

    Integer requiredPoints
) implements Serializable {

}
