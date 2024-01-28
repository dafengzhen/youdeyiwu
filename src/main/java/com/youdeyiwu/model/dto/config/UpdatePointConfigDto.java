package com.youdeyiwu.model.dto.config;

import java.io.Serializable;

/**
 * update point config.
 *
 * @param enable     enable
 * @param initPoints initPoints
 */
public record UpdatePointConfigDto(
    Boolean enable,

    Integer initPoints
) implements Serializable {

}