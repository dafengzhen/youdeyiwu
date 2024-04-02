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
    ConfigEntity configEntity = configRepository.findByTypeAndName(
        ConfigTypeEnum.POST,
        PostConfigConstant.createGuide
    );

    return configEntity.getValue();
  }

  @Transactional
  @Override
  public void updateCreateGuide(UpdateCreateGuidePostConfigDto dto) {
    ConfigEntity configEntity = configRepository.findByTypeAndName(
        ConfigTypeEnum.POST,
        PostConfigConstant.createGuide
    );

    if (Objects.nonNull(dto.createGuide())) {
      configEntity.setValue(cleanHtmlContent(dto.createGuide().trim()));
    }
  }
}