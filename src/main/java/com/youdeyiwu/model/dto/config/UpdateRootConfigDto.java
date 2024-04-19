package com.youdeyiwu.model.dto.config;

import java.io.Serializable;

/**
 * update root config.
 *
 * @param secret                   secret
 * @param disableRegistration      disableRegistration
 * @param disableAnonymousPosts    disableAnonymousPosts
 * @param disableAnonymousComments disableAnonymousComments
 * @param disableAnonymousReplies  disableAnonymousReplies
 */
public record UpdateRootConfigDto(
    String secret,

    Boolean disableRegistration,

    Boolean disableAnonymousPosts,

    Boolean disableAnonymousComments,

    Boolean disableAnonymousReplies
) implements Serializable {

}