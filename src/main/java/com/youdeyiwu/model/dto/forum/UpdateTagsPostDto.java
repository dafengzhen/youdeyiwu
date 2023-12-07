package com.youdeyiwu.model.dto.forum;

import java.io.Serializable;
import java.util.Set;

/**
 * update tags post.
 *
 * @param tags tags
 */
public record UpdateTagsPostDto(
    Set<Long> tags
) implements Serializable {

}
