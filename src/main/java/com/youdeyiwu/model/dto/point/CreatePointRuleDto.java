package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.RuleNameEnum;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * create point rule.
 *
 * @param ruleName       ruleName
 * @param requiredPoints requiredPoints
 */
public record CreatePointRuleDto(
    @NotNull
    RuleNameEnum ruleName,

    @NotNull
    Integer requiredPoints
) implements Serializable {

}
