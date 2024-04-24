package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;

/**
 * update styles.
 *
 * @param styles     styles
 * @param classNames classNames
 */
public record UpdateStylesPostDto(
    String styles,

    String classNames
) implements Serializable {

}
