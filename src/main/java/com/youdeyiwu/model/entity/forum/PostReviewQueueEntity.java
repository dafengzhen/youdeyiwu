package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.Entity;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import java.time.LocalDate;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * post review queue.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PostReviewQueueEntity extends AbstractEntity {

  /**
   * received.
   */
  private Boolean received = true;

  /**
   * latest review result time (The result time will be displayed to the user for viewing).
   */
  private LocalDate latestReviewResultTime = LocalDate.now();

  /**
   * receiver.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity receiver;

  /**
   * post.
   */
  @OneToOne
  @JsonIgnore
  @ToString.Exclude
  private PostEntity post;

}