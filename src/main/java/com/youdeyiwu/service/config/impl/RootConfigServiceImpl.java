package com.youdeyiwu.service.config.impl;

import static com.youdeyiwu.tool.Tool.randomUuId;

import com.youdeyiwu.constant.RootConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.config.UpdateRootConfigDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.config.RootConfigVo;
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
      String secret = configRepository.findOptionalByTypeAndName(
              ConfigTypeEnum.ROOT,
              RootConfigConstant.SECRET
          )
          .map(ConfigEntity::getValue)
          .orElse(null);

      if (Objects.nonNull(secret) && !secret.equals(dto.secret())) {
        throw new CustomException(i18nTool.getMessage("config.root.secret.invalid"));
      }

      configRepository.saveByTypeAndName(
          ConfigTypeEnum.ROOT,
          RootConfigConstant.SECRET,
          randomUuId()
      );

      UserEntity userEntity = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      userEntity.setRoot(true);
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

    handleAnonymousCreationConfiguration(dto);
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

  @Override
  public RootConfigVo query(Boolean disableRegistration, Boolean disableAnonymous) {
    RootConfigVo vo = new RootConfigVo();

    if (Boolean.TRUE.equals(disableRegistration)) {
      vo.setDisableRegistration(queryDisableRegistration());
    }

    if (Boolean.TRUE.equals(disableAnonymous)) {
      queryAnonymousCreationConfiguration(vo);
    }

    return vo;
  }

  /**
   * handle anonymous creation configuration.
   *
   * @param dto dto
   */
  private void handleAnonymousCreationConfiguration(UpdateRootConfigDto dto) {
    if (Objects.nonNull(dto.disableAnonymousPosts())) {
      configRepository.saveByTypeAndName(
          ConfigTypeEnum.ROOT,
          RootConfigConstant.DISABLE_ANONYMOUS_POSTS,
          dto.disableAnonymousPosts().toString()
      );
    }

    if (Objects.nonNull(dto.disableAnonymousComments())) {
      configRepository.saveByTypeAndName(
          ConfigTypeEnum.ROOT,
          RootConfigConstant.DISABLE_ANONYMOUS_COMMENTS,
          dto.disableAnonymousComments().toString()
      );
    }

    if (Objects.nonNull(dto.disableAnonymousReplies())) {
      configRepository.saveByTypeAndName(
          ConfigTypeEnum.ROOT,
          RootConfigConstant.DISABLE_ANONYMOUS_REPLIES,
          dto.disableAnonymousReplies().toString()
      );
    }
  }

  /**
   * query anonymous creation configuration.
   *
   * @param vo vo
   */
  private void queryAnonymousCreationConfiguration(RootConfigVo vo) {
    vo.setDisableAnonymousPosts(
        configRepository.findOptionalByTypeAndName(
                ConfigTypeEnum.ROOT,
                RootConfigConstant.DISABLE_ANONYMOUS_POSTS
            )
            .map(configEntity -> Boolean.valueOf(configEntity.getValue()))
            .orElse(false)
    );

    vo.setDisableAnonymousComments(
        configRepository.findOptionalByTypeAndName(
                ConfigTypeEnum.ROOT,
                RootConfigConstant.DISABLE_ANONYMOUS_COMMENTS
            )
            .map(configEntity -> Boolean.valueOf(configEntity.getValue()))
            .orElse(false)
    );

    vo.setDisableAnonymousReplies(
        configRepository.findOptionalByTypeAndName(
                ConfigTypeEnum.ROOT,
                RootConfigConstant.DISABLE_ANONYMOUS_REPLIES
            )
            .map(configEntity -> Boolean.valueOf(configEntity.getValue()))
            .orElse(false)
    );
  }
}