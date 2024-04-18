package com.youdeyiwu.model.dto.config;

import java.io.Serializable;

/**
 * update root config.
 *
 * @param secret              secret
 * @param disableRegistration disableRegistration
 */
public record UpdateRootConfigDto(
    String secret,

    Boolean disableRegistration
) implements Serializable {

}