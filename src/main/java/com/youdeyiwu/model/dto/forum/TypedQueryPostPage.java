package com.youdeyiwu.model.dto.forum;

import com.youdeyiwu.model.entity.forum.PostEntity;
import jakarta.persistence.TypedQuery;

/**
 * post page.
 *
 * @param query          query
 * @param totalSizeQuery totalSizeQuery
 */
public record TypedQueryPostPage(
    TypedQuery<PostEntity> query,

    TypedQuery<Long> totalSizeQuery
) {

}
