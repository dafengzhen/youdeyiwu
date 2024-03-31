package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.RuleNameEnum;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * save point rule.
 *
 * @param ruleName              ruleName
 * @param initiatorRewardPoints initiatorRewardPoints
 * @param receiverRewardPoints  receiverRewardPoints
 * @param enable                enable
 */
public record SavePointRuleDto(
    @NotNull(message = "{point.ruleName.required}")
    RuleNameEnum ruleName,

    Integer initiatorRewardPoints,

    Integer receiverRewardPoints,

    Boolean enable
) implements Serializable {

}
