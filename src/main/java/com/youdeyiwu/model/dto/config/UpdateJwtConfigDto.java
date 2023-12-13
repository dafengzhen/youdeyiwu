package com.youdeyiwu.model.dto.config;

import java.io.Serializable;

/**
 * update jwt config.
 *
 * @param secret secret
 */
public record UpdateJwtConfigDto(
    String secret
) implements Serializable {

}