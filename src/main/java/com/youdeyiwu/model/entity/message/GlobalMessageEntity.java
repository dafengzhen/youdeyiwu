package com.youdeyiwu.model.entity.message;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.message.MessageRangeEnum;
import com.youdeyiwu.enums.message.MessageTypeEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Enumerated;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/**
 * global message.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class GlobalMessageEntity extends AbstractEntity {

  /**
   * name.
   */
  @Column(nullable = false)
  private String name;

  /**
   * overview.
   */
  @Column(nullable = false, length = 512)
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
  private MessageTypeEnum messageType = MessageTypeEnum.GLOBAL_MESSAGE;

  /**
   * message range.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private MessageRangeEnum messageRange = MessageRangeEnum.ALL_USER;

  /**
   * sort.
   */
  @Column(nullable = false)
  private Integer sort = 0;

  /**
   * sender.
   */
  @OneToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity sender;

  /**
   * global message users.
   */
  @OneToMany(
      mappedBy = "globalMessage",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<GlobalMessageUserEntity> globalMessageUsers = new HashSet<>();

}