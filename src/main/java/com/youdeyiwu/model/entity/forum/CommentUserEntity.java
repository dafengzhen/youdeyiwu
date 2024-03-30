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
 * comment user.
 *
 * @author dafengzhen
 */
@SuppressWarnings("com.haulmont.jpb.MoreThanOneIdInspection")
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class CommentUserEntity extends IdLessAbstractEntity {

  /**
   * liked.
   */
  private Boolean liked = false;

  /**
   * comment.
   */
  @Id
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private CommentEntity comment;

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
    if (!(o instanceof CommentUserEntity that)) {
      return false;
    }
    return Objects.equals(comment, that.comment) && Objects.equals(user, that.user);
  }

  @Override
  public int hashCode() {
    return Objects.hash(comment, user);
  }
}