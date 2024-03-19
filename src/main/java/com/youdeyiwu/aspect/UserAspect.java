package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.model.dto.user.LoginDto;
import com.youdeyiwu.model.dto.user.RegisterDto;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.TokenVo;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.tool.I18nTool;
import java.util.HashMap;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

/**
 * user.
 *
 * @author dafengzhen
 */
@Aspect
@Component
@RequiredArgsConstructor
public class UserAspect {

  private final ApplicationEventPublisher publisher;

  private final UserRepository userRepository;

  private final I18nTool i18nTool;

  /**
   * register.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.user.impl.UserServiceImpl.register(..)) && args(dto)", argNames = "dto")
  public void registerPointcut(RegisterDto dto) {
    // Pointcut
  }

  /**
   * login.
   */
  @Pointcut(value = "execution(* com.youdeyiwu.service.user.impl.UserServiceImpl.login(..)) && args(dto)", argNames = "dto")
  public void loginPointcut(LoginDto dto) {
    // Pointcut
  }

  /**
   * after returning advice.
   */
  @AfterReturning(value = "registerPointcut(dto)", returning = "vo", argNames = "vo,dto")
  public void registerAfterReturningAdvice(TokenVo vo, RegisterDto dto) {
    UserEntity userEntity = userRepository.getReferenceById(vo.getId());
    MessageEntity messageEntity = createMessageEntity(
        userEntity,
        "user.register.message.name",
        "user.register.message.overview"
    );
    messageEntity.setSender(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * after returning advice.
   */
  @AfterReturning(value = "loginPointcut(dto)", returning = "vo", argNames = "vo,dto")
  public void loginAfterReturningAdvice(TokenVo vo, LoginDto dto) {
    UserEntity userEntity = userRepository.getReferenceById(vo.getId());
    MessageEntity messageEntity = createMessageEntity(
        userEntity,
        "user.login.message.name",
        "user.login.message.overview"
    );
    messageEntity.setSender(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * create message entity.
   *
   * @param userEntity       userEntity
   * @param eventNameKey     eventNameKey
   * @param eventOverviewKey eventOverviewKey
   * @return MessageEntity
   */
  private MessageEntity createMessageEntity(UserEntity userEntity, String eventNameKey, String eventOverviewKey) {
    Map<String, Object> overviewArgs = new HashMap<>();
    overviewArgs.put("time", userEntity.getId() + "#" + getCurrentDateTime());

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName(i18nTool.getMessage(eventNameKey));
    messageEntity.setOverview(i18nTool.getMessage(eventOverviewKey, overviewArgs));
    messageEntity.setLink(userEntity.getLink());
    messageEntity.setReceiver(userEntity);
    return messageEntity;
  }
}
