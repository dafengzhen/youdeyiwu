package com.youdeyiwu.service.config.impl;

import static com.youdeyiwu.tool.Tool.randomUuId;

import com.youdeyiwu.constant.RootConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.config.UpdateRootConfigDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.user.RoleRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.config.RootConfigService;
import com.youdeyiwu.tool.I18nTool;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * root.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class RootConfigServiceImpl implements RootConfigService {

  private final ConfigRepository configRepository;

  private final UserRepository userRepository;

  private final SecurityService securityService;

  private final I18nTool i18nTool;

  private final RoleRepository roleRepository;

  @Transactional
  @Override
  public void update(UpdateRootConfigDto dto) {
    if (StringUtils.hasText(dto.secret())) {
      ConfigEntity configEntity = configRepository.findByTypeAndName(
          ConfigTypeEnum.ROOT,
          RootConfigConstant.SECRET
      );

      if (!Objects.equals(configEntity.getValue(), dto.secret())) {
        throw new CustomException(i18nTool.getMessage("config.root.secret.invalid"));
      }

      UserEntity userEntity = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      userEntity.setRoot(true);
      configEntity.setValue(randomUuId());

      roleRepository.findById(1L)
          .ifPresent(roleEntity -> {
            roleEntity.getUsers().add(userEntity);
            userEntity.getRoles().add(roleEntity);
          });
    }

    if (Objects.nonNull(dto.disableRegistration())) {
      configRepository.saveByTypeAndName(
          ConfigTypeEnum.ROOT,
          RootConfigConstant.DISABLE_REGISTRATION,
          dto.disableRegistration().toString()
      );
    }
  }

  @Override
  public Boolean queryDisableRegistration() {
    return configRepository.findOptionalByTypeAndName(
            ConfigTypeEnum.ROOT,
            RootConfigConstant.DISABLE_REGISTRATION
        )
        .map(configEntity -> Boolean.valueOf(configEntity.getValue()))
        .orElse(false);
  }
}