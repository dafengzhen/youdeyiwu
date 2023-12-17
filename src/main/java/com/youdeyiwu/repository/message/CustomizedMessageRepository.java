package com.youdeyiwu.repository.message;

import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.entity.message.GlobalMessageUserEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import org.springframework.data.domain.Page;

/**
 * message.
 *
 * @author dafengzhen
 */
public interface CustomizedMessageRepository {

  /**
   * findAllByReceiver.
   *
   * @param receiver receiver
   * @param position position
   * @return Page
   */
  Page<GlobalMessageUserEntity> findAllByReceiver(
      UserEntity receiver,
      PaginationPositionDto position
  );
}
