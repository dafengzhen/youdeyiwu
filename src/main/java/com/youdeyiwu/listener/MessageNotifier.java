package com.youdeyiwu.listener;

import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.repository.message.MessageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 * message listener.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Component
public class MessageNotifier implements ApplicationListener<MessageApplicationEvent> {

  private final MessageRepository messageRepository;

  @Override
  public void onApplicationEvent(MessageApplicationEvent event) {
    MessageEntity entity = (MessageEntity) event.getSource();
    messageRepository.save(entity);
  }
}
