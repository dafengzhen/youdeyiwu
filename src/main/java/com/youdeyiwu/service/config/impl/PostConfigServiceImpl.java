package com.youdeyiwu.service.config.impl;

import static com.youdeyiwu.tool.Tool.cleanHtmlContent;

import com.youdeyiwu.constant.PostConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.model.dto.config.UpdateCreateGuidePostConfigDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.service.config.PostConfigService;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * post.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class PostConfigServiceImpl implements PostConfigService {

  private final ConfigRepository configRepository;

  @Override
  public String queryCreateGuide() {
    return configRepository.findOptionalByTypeAndName(
            ConfigTypeEnum.POST,
            PostConfigConstant.createGuide
        )
        .map(ConfigEntity::getValue)
        .orElse(null);
  }

  @Transactional
  @Override
  public void updateCreateGuide(UpdateCreateGuidePostConfigDto dto) {
    if (Objects.nonNull(dto.createGuide())) {
      configRepository.saveByTypeAndName(
          ConfigTypeEnum.POST,
          PostConfigConstant.createGuide,
          cleanHtmlContent(dto.createGuide().trim())
      );
    }
  }
}