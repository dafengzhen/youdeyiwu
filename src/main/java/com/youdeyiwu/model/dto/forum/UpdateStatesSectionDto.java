package com.youdeyiwu.model.dto.forum;

import com.youdeyiwu.enums.forum.SectionStateEnum;
import java.io.Serializable;
import java.util.Set;

/**
 * update states section.
 *
 * @param states    states
 * @param allows    allows
 * @param blocks    blocks
 * @param accessKey accessKey
 */
public record UpdateStatesSectionDto(
    Set<SectionStateEnum> states,

    Set<Long> allows,

    Set<Long> blocks,

    String accessKey
) implements Serializable {

}
