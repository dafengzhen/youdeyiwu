package com.youdeyiwu.enums.permission;

import lombok.Getter;

/**
 * matcher type.
 * If you need to add an enumeration, please append it at the end.
 * This is because the enumeration is stored in natural order, not by name.
 *
 * @author dafengzhen
 */
@Getter
public enum MatcherTypeEnum {

    /**
     * ant.
     */
    ANT,

    /**
     * regex.
     */
    REGEX

}
