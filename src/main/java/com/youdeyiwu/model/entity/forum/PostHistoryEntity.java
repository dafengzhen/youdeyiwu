package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.Entity;
import jakarta.persistence.ManyToOne;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * post history.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PostHistoryEntity extends AbstractEntity {

  /**
   * disable comments.
   */
  private Boolean disableComments = false;

  /**
   * disable replies.
   */
  private Boolean disableReplies = false;

  /**
   * comment disable reason.
   */
  private String commentDisableReason;

  /**
   * reply disable reason.
   */
  private String replyDisableReason;

  /**
   * post.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private PostEntity post;

}