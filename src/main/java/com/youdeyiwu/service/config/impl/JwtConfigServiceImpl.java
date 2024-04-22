package com.youdeyiwu.service.config.impl;

import static com.youdeyiwu.tool.JwtTool.createJwt;
import static com.youdeyiwu.tool.JwtTool.decodeSecret;
import static com.youdeyiwu.tool.JwtTool.encodeSecret;

import com.youdeyiwu.constant.JwtConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.model.dto.config.UpdateJwtConfigDto;
import com.youdeyiwu.model.entity.config.ConfigEntity;
import com.youdeyiwu.model.vo.config.JwtConfigVo;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.service.config.JwtConfigService;
import io.jsonwebtoken.JwtException;
import java.time.Duration;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * jwt.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class JwtConfigServiceImpl implements JwtConfigService {

  private final ConfigRepository configRepository;

  @Override
  public String generateRandomSecret() {
    return encodeSecret();
  }

  @Override
  public JwtConfigVo query() {
    String secret = configRepository.findOptionalByTypeAndName(
            ConfigTypeEnum.JWT,
            JwtConfigConstant.SECRET
        )
        .map(ConfigEntity::getValue)
        .orElse(null);

    JwtConfigVo vo = new JwtConfigVo();
    vo.setSecret(secret);
    return vo;
  }

  @Transactional
  @Override
  public void update(UpdateJwtConfigDto dto) {
    if (StringUtils.hasText(dto.secret())) {
      try {
        createJwt(decodeSecret(dto.secret()), 0L, Duration.ofDays(1));
      } catch (JwtException e) {
        throw new CustomException(e.getMessage());
      }

      configRepository.saveByTypeAndName(
          ConfigTypeEnum.JWT,
          JwtConfigConstant.SECRET,
          dto.secret()
      );
    }
  }
}