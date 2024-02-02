package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * save point auto rule.
 *
 * @param autoRuleName   autoRuleName
 * @param requiredPoints requiredPoints
 */
public record SavePointAutoRuleDto(
    @NotNull
    AutoRuleNameEnum autoRuleName,

    Integer requiredPoints
) implements Serializable {

}
