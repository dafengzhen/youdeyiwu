package com.youdeyiwu.model.vo.message;

import com.youdeyiwu.enums.message.MessageRangeEnum;
import com.youdeyiwu.enums.message.MessageStateEnum;
import com.youdeyiwu.enums.message.MessageTypeEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import java.util.Map;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * message.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class MessageEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * overview.
   */
  private String overview;

  /**
   * content.
   */
  private Map<String, String> content;

  /**
   * message type.
   */
  private MessageTypeEnum messageType;

  /**
   * message range.
   */
  private MessageRangeEnum messageRange;

  /**
   * state.
   */
  private MessageStateEnum state;

  /**
   * sender.
   */
  private UserEntityVo sender;

  /**
   * receiver.
   */
  private UserEntityVo receiver;

}