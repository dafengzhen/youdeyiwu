package com.youdeyiwu.model.dto.point;

import java.io.Serializable;

/**
 * update point.
 *
 * @param points    points
 * @param minPoints minPoints
 * @param maxPoints maxPoints
 */
public record UpdatePointDto(
    Integer points,

    Integer minPoints,

    Integer maxPoints
) implements Serializable {

}
