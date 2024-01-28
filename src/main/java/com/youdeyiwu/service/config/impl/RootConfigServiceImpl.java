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
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.config.RootConfigService;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * root.
 *
 * @author dafengzhen
 */
@Log4j2
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class RootConfigServiceImpl implements RootConfigService {

  private final ConfigRepository configRepository;

  private final UserRepository userRepository;

  private final SecurityService securityService;

  @Transactional
  @Override
  public void update(UpdateRootConfigDto dto) {
    if (StringUtils.hasText(dto.secret())) {
      ConfigEntity configEntity = configRepository.findByTypeAndName(
          ConfigTypeEnum.ROOT,
          RootConfigConstant.SECRET
      );

      if (!Objects.equals(configEntity.getValue(), dto.secret())) {
        throw new CustomException(
            "Apologies, the secret cannot be decrypted, setting it as the forum administrator has failed");
      }

      UserEntity userEntity = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      userEntity.setRoot(true);
      configEntity.setValue(randomUuId());
      log.info(
          "=== Config === Update root.secret option === Operating user ID: {}",
          userEntity.getId()
      );
    }
  }
}