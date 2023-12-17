package com.youdeyiwu.enums.message;

import lombok.Getter;

/**
 * message range.
 * If you need to add an enumeration, please append it at the end.
 * This is because the enumeration is stored in natural order, not by name.
 *
 * @author dafengzhen
 */
@Getter
public enum MessageRangeEnum {

    /**
     * all user.
     */
    ALL_USER,

    /**
     * user.
     */
    USER

}
