package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.AutoRuleNameEnum;
import com.youdeyiwu.enums.point.SignEnum;
import java.io.Serializable;

/**
 * point auto rule.
 *
 * @param autoRuleName autoRuleName
 * @param sign         sign
 * @param postId       postId
 */
public record PointAutoRuleEventDto(
    AutoRuleNameEnum autoRuleName,

    SignEnum sign,

    Long postId
) implements Serializable {

}
