package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * create point auto rule.
 *
 * @param autoRuleName   autoRuleName
 * @param requiredPoints requiredPoints
 */
public record CreatePointAutoRuleDto(
    @NotNull
    AutoRuleNameEnum autoRuleName,

    @NotNull
    Integer requiredPoints
) implements Serializable {

}
