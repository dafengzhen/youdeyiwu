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
 * @param from               from
 * @param link               link
 * @param receivedUserIds    receivedUserIds
 * @param checkHistoryPoints checkHistoryPoints
 */
public record PointRuleEventDto(
    RuleNameEnum ruleName,

    SignEnum sign,

    String from,

    String link,

    Set<Long> receivedUserIds,

    Boolean checkHistoryPoints
) implements Serializable {

}
