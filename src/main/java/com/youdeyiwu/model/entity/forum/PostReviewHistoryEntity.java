package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.forum.PostReviewStateEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Enumerated;
import jakarta.persistence.ManyToOne;
import java.time.LocalDate;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * post review history.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PostReviewHistoryEntity extends AbstractEntity {

  /**
   * review state.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private PostReviewStateEnum reviewState = PostReviewStateEnum.APPROVED;

  /**
   * review reason.
   */
  private String reviewReason;

  /**
   * latest review result time (The result time will be displayed to the user for viewing).
   */
  private LocalDate latestReviewResultTime = LocalDate.now();

  /**
   * reviewer.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity reviewer;

  /**
   * post.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private PostEntity post;

}