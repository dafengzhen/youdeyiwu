package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.IdLessAbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.ManyToOne;
import java.util.Objects;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * quote reply user.
 *
 * @author dafengzhen
 */
@SuppressWarnings("com.haulmont.jpb.MoreThanOneIdInspection")
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class QuoteReplyUserEntity extends IdLessAbstractEntity {

  /**
   * liked.
   */
  private Boolean liked = false;

  /**
   * quote reply.
   */
  @Id
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private QuoteReplyEntity quoteReply;

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
    if (!(o instanceof QuoteReplyUserEntity that)) {
      return false;
    }
    return Objects.equals(quoteReply, that.quoteReply) && Objects.equals(user, that.user);
  }

  @Override
  public int hashCode() {
    return Objects.hash(quoteReply, user);
  }
}