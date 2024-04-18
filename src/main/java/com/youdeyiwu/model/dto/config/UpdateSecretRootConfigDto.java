package com.youdeyiwu.model.dto.config;

import java.io.Serializable;

/**
 * update secret.
 *
 * @param secret secret
 */
public record UpdateSecretRootConfigDto(
    String secret
) implements Serializable {

}