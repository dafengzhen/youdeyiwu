package com.youdeyiwu.model.vo.message;

import com.youdeyiwu.enums.message.MessageRangeEnum;
import com.youdeyiwu.enums.message.MessageStateEnum;
import com.youdeyiwu.enums.message.MessageTypeEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import java.util.Map;
import java.util.Objects;
import lombok.Data;

/**
 * message.
 *
 * @author dafengzhen
 */
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
   * link (Can be an absolute or relative path).
   */
  private String link;

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

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    if (!super.equals(o)) {
      return false;
    }
    MessageEntityVo that = (MessageEntityVo) o;
    return messageRange == that.messageRange;
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode(), messageRange);
  }
}