package com.youdeyiwu.listener;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.message.MessageRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * message listener.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class MessageNotifier implements ApplicationListener<MessageApplicationEvent> {

  private final MessageRepository messageRepository;

  private final UserRepository userRepository;

  private final SecurityService securityService;

  @Transactional
  @Override
  public void onApplicationEvent(MessageApplicationEvent event) {
    MessageEntity entity = (MessageEntity) event.getSource();
    UserEntity sender = entity.getSender();

    if (Objects.isNull(sender) && (!securityService.isAnonymous())) {
      sender = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
    }

    entity.setSender(sender);
    messageRepository.save(entity);
  }
}
