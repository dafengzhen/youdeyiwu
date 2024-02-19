package com.youdeyiwu.repository.message;

import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * message.
 *
 * @author dafengzhen
 */
public interface MessageRepository extends JpaRepositoryImplementation<MessageEntity, Long> {

  /**
   * findByIdAndSenderIsNull.
   *
   * @param id id
   * @return MessageEntity
   */
  MessageEntity findByIdAndSenderIsNull(Long id);

  /**
   * findByIdAndReceiver.
   *
   * @param id       id
   * @param receiver receiver
   * @return MessageEntity
   */
  MessageEntity findByIdAndReceiver(Long id, UserEntity receiver);

  /**
   * findAllByReceiver.
   *
   * @param userEntity userEntity
   * @param pageable   pageable
   * @return Page
   */
  Page<MessageEntity> findAllByReceiver(UserEntity userEntity, Pageable pageable);

  /**
   * findAllBySenderIsNull.
   *
   * @param pageable pageable
   * @return Page
   */
  Page<MessageEntity> findAllBySenderIsNull(Pageable pageable);
}
