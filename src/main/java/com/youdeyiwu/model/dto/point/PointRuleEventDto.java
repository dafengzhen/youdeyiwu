package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.enums.point.SignEnum;
import java.io.Serializable;
import java.util.Set;

/**
 * point rule.
 *
 * @param ruleName           ruleName
 * @param sign               sign
 * @param onlyInitiator      onlyInitiator
 * @param from               from
 * @param link               link
 * @param postId             postId
 * @param receiverUserIds    receiverUserIds
 * @param checkHistoryPoints checkHistoryPoints
 */
public record PointRuleEventDto(
    RuleNameEnum ruleName,

    SignEnum sign,

    Boolean onlyInitiator,

    String from,

    String link,

    Long postId,

    Set<Long> receiverUserIds,

    Boolean checkHistoryPoints
) implements Serializable {

}
