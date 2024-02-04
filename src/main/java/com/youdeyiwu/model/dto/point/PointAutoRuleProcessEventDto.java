package com.youdeyiwu.model.dto.point;

import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import java.io.Serializable;

/**
 * point auto rule.
 *
 * @param userEntity         userEntity
 * @param updatedPointEntity updatedPointEntity
 */
public record PointAutoRuleProcessEventDto(
    UserEntity userEntity,

    PointEntity updatedPointEntity
) implements Serializable {

}
