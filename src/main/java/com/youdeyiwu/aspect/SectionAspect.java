package com.youdeyiwu.aspect;

import com.youdeyiwu.constant.PointConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.SectionNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.forum.SectionRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.tool.I18nTool;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

/**
 * section.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class SectionAspect {

  private final I18nTool i18nTool;

  private final SecurityService securityService;

  private final UserRepository userRepository;

  private final SectionRepository sectionRepository;

  private final ConfigRepository configRepository;

  /**
   * queryDetails.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.forum.impl.SectionServiceImpl.queryDetails(..)) && args(id,sectionKey)", argNames = "id,sectionKey")
  public void queryDetailsPointcut(Long id, String sectionKey) {
    // Pointcut
  }

  /**
   * before advice.
   */
  @Before(value = "queryDetailsPointcut(id,sectionKey)", argNames = "id,sectionKey")
  public void queryDetailsBeforeAdvice(Long id, String sectionKey) {
    Boolean enable = configRepository.findOptionalByTypeAndName(
            ConfigTypeEnum.POINT,
            PointConfigConstant.ENABLE
        )
        .map(configEntity -> Boolean.valueOf(configEntity.getValue()))
        .orElse(false);

    if (Boolean.FALSE.equals(enable)) {
      return;
    }

    SectionEntity sectionEntity = sectionRepository.findById(id)
        .orElseThrow(SectionNotFoundException::new);
    Integer accessPoints = sectionEntity.getAccessPoints();

    if (Objects.isNull(accessPoints) || accessPoints == 0) {
      return;
    }

    if (securityService.isAnonymous()) {
      throw new CustomException(
          i18nTool.getMessage(
              "section.accessPoints.error",
              Map.of(
                  "point", accessPoints,
                  "currentPoint", "0"
              )
          )
      );
    }

    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    PointEntity pointEntity = userEntity.getPoint();
    if (Objects.isNull(pointEntity) || pointEntity.getPoints() < accessPoints) {
      throw new CustomException(
          i18nTool.getMessage(
              "section.accessPoints.error",
              Map.of(
                  "point", accessPoints,
                  "currentPoint", "0"
              )
          )
      );
    }
  }
}
