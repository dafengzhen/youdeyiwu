package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.forum.CommentReviewStateEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Enumerated;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import java.util.HashSet;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * comment.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class CommentEntity extends AbstractEntity {

  /**
   * content.
   */
  @Column(nullable = false)
  private String content;

  /**
   * likes count.
   */
  private Long likesCount = 0L;

  /**
   * reviewState.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private CommentReviewStateEnum reviewState = CommentReviewStateEnum.APPROVED;

  /**
   * post.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private PostEntity post;

  /**
   * user.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity user;

  /**
   * quote replies.
   */
  @OneToMany(
      mappedBy = "comment",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<QuoteReplyEntity> quoteReplies = new HashSet<>();

  /**
   * unique identifier.
   */
  private String uniqueIdentifier;

}