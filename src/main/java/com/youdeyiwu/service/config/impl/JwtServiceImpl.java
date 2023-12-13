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
import com.youdeyiwu.service.config.JwtService;
import io.jsonwebtoken.JwtException;
import java.time.Duration;
import java.util.Optional;
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
public class JwtServiceImpl implements JwtService {

  private final ConfigRepository configRepository;

  @Override
  public String generateRandomSecret() {
    return encodeSecret();
  }

  @Override
  public JwtConfigVo query() {
    ConfigEntity configEntity = configRepository.findByTypeAndName(
        ConfigTypeEnum.JWT,
        JwtConfigConstant.SECRET
    );

    JwtConfigVo vo = new JwtConfigVo();
    vo.setSecret(configEntity.getValue());
    return vo;
  }

  @Transactional
  @Override
  public void update(UpdateJwtConfigDto dto) {
    if (StringUtils.hasText(dto.secret())) {
      try {
        createJwt(decodeSecret(dto.secret()), 0L, Duration.ofDays(1));
        Optional<ConfigEntity> configEntityOptional = Optional.ofNullable(
            configRepository.findByTypeAndName(
                ConfigTypeEnum.JWT,
                JwtConfigConstant.SECRET
            )
        );
        if (configEntityOptional.isEmpty()) {
          ConfigEntity configEntity = new ConfigEntity();
          configEntity.setType(ConfigTypeEnum.JWT);
          configEntity.setName(JwtConfigConstant.SECRET);
          configEntity.setValue(dto.secret());
          configRepository.save(configEntity);
        } else {
          configEntityOptional.get().setValue(dto.secret());
        }
      } catch (JwtException e) {
        throw new CustomException("Invalid JWT token secret");
      }
    }
  }
}