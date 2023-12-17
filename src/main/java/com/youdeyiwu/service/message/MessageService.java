package com.youdeyiwu.service.message;

import com.youdeyiwu.model.dto.message.CreateGlobalMessageDto;
import com.youdeyiwu.model.dto.message.CreateMessageDto;
import com.youdeyiwu.model.entity.message.GlobalMessageEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.message.GlobalMessageEntityVo;
import com.youdeyiwu.model.vo.message.MessageEntityVo;
import org.springframework.data.domain.Pageable;

/**
 * menu.
 *
 * @author dafengzhen
 */
public interface MessageService {

  /**
   * create global message.
   *
   * @param dto dto
   * @return GlobalMessageEntity
   */
  GlobalMessageEntity createGlobalMessage(CreateGlobalMessageDto dto);

  /**
   * create.
   *
   * @param dto dto
   * @return MessageEntity
   */
  MessageEntity create(CreateMessageDto dto);

  /**
   * updateState global message.
   *
   * @param id id
   */
  void updateGlobalMessageState(Long id);

  /**
   * updateState.
   *
   * @param id id
   */
  void updateState(Long id);

  /**
   * query global message.
   *
   * @param id id
   * @return GlobalMessageEntityVo
   */
  GlobalMessageEntityVo queryGlobalMessage(Long id);

  /**
   * query.
   *
   * @param id id
   * @return MessageEntityVo
   */
  MessageEntityVo query(Long id);

  /**
   * query all global message.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<GlobalMessageEntityVo> queryAllGlobalMessage(Pageable pageable);

  /**
   * query all global message.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<MessageEntityVo> queryAll(Pageable pageable);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}