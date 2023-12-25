package com.youdeyiwu.model.dto.config;

import java.io.Serializable;

/**
 * update root config.
 *
 * @param secret secret
 */
public record UpdateRootConfigDto(
    String secret
) implements Serializable {

}