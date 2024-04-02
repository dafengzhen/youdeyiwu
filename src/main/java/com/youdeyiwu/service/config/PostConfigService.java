package com.youdeyiwu.service.config;

import com.youdeyiwu.model.dto.config.UpdateCreateGuidePostConfigDto;

/**
 * post.
 *
 * @author dafengzhen
 */
public interface PostConfigService {

  /**
   * query create guide.
   *
   * @return String
   */
  String queryCreateGuide();

  /**
   * update create guide.
   *
   * @param dto dto
   */
  void updateCreateGuide(UpdateCreateGuidePostConfigDto dto);
}