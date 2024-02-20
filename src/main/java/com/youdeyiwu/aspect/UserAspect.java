package com.youdeyiwu.aspect;

import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.model.dto.user.LoginDto;
import com.youdeyiwu.model.dto.user.RegisterDto;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.TokenVo;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
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

  private final SecurityService securityService;

  private final UserRepository userRepository;

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
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("Welcome notification");
    messageEntity.setOverview(
        """
            Welcome, %s!
            Thank you for registering with us.
            Your registration was successful at %s.
            We are excited to have you on board!
            """
            .formatted(securityService.getAliasAndId(userEntity), getCurrentDateTime())
    );
    messageEntity.setLink("/users/" + userEntity.getId());
    messageEntity.setReceiver(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }

  /**
   * after returning advice.
   */
  @AfterReturning(value = "loginPointcut(dto)", returning = "vo", argNames = "vo,dto")
  public void loginAfterReturningAdvice(TokenVo vo, LoginDto dto) {
    UserEntity userEntity = userRepository.getReferenceById(vo.getId());
    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("Login notification");
    messageEntity.setOverview(
        """
            Congratulations on your successful login.
            You performed an operation at %s time
            """
            .formatted(getCurrentDateTime())
    );
    messageEntity.setReceiver(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
  }
}
