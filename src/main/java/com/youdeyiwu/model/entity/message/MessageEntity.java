package com.youdeyiwu.model.entity.message;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.message.MessageRangeEnum;
import com.youdeyiwu.enums.message.MessageStateEnum;
import com.youdeyiwu.enums.message.MessageTypeEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Enumerated;
import jakarta.persistence.ManyToOne;
import java.util.HashMap;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/**
 * message.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class MessageEntity extends AbstractEntity {

  /**
   * name.
   */
  @Column(nullable = false)
  private String name;

  /**
   * overview.
   */
  @Column(nullable = false)
  private String overview;

  /**
   * link (Can be an absolute or relative path).
   */
  private String link;

  /**
   * content.
   */
  @JdbcTypeCode(SqlTypes.JSON)
  private Map<String, String> content = new HashMap<>();

  /**
   * message type.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private MessageTypeEnum messageType = MessageTypeEnum.MESSAGE;

  /**
   * message range.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private MessageRangeEnum messageRange = MessageRangeEnum.USER;

  /**
   * state.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private MessageStateEnum state = MessageStateEnum.UNREAD;

  /**
   * sender.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity sender;

  /**
   * receiver.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity receiver;

}