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
 * global message.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class GlobalMessageEntityVo extends AbstractEntityVo {

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
   * sort.
   */
  private Integer sort;

  /**
   * sender.
   */
  private UserEntityVo sender;

}