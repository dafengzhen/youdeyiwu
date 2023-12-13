package com.youdeyiwu.model.entity.message;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.message.MessageStateEnum;
import com.youdeyiwu.model.entity.IdLessAbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.ManyToOne;
import java.util.Objects;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * global message user.
 *
 * @author dafengzhen
 */
@SuppressWarnings("com.haulmont.jpb.MoreThanOneIdInspection")
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class GlobalMessageUserEntity extends IdLessAbstractEntity {

  /**
   * state.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private MessageStateEnum state = MessageStateEnum.UNREAD;

  /**
   * global message.
   */
  @Id
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private GlobalMessageEntity globalMessage;

  /**
   * user.
   */
  @Id
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity user;

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (!(o instanceof GlobalMessageUserEntity that)) {
      return false;
    }
    return Objects.equals(globalMessage, that.globalMessage) && Objects.equals(user, that.user);
  }

  @Override
  public int hashCode() {
    return Objects.hash(globalMessage, user);
  }
}