package com.youdeyiwu.model.dto.config;

import java.io.Serializable;

/**
 * update create guide.
 *
 * @param createGuide createGuide
 */
public record UpdateCreateGuidePostConfigDto(
    String createGuide
) implements Serializable {

}