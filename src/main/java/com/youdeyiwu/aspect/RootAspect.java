package com.youdeyiwu.aspect;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.config.UpdateRootConfigDto;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.tool.I18nTool;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

/**
 * root.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class RootAspect {

  private final ApplicationEventPublisher publisher;

  private final UserRepository userRepository;

  private final I18nTool i18nTool;

  private final SecurityService securityService;

  /**
   * updateStates.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.config.impl.RootConfigServiceImpl.update(..)) && args(dto)", argNames = "dto")
  public void updatePointcut(UpdateRootConfigDto dto) {
    // Pointcut
  }

  /**
   * after advice.
   */
  @After(value = "updatePointcut(dto)", argNames = "dto")
  public void updateStatesAfterAdvice(UpdateRootConfigDto dto) {
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setOverview(i18nTool.getMessage("config.root.update.overview"));
    messageEntity.setLink(userEntity.getLink());
    messageEntity.setReceiver(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }
}
