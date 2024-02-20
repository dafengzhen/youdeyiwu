package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.forum.QuoteReplyReviewStateEnum;
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
 * quote reply.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class QuoteReplyEntity extends AbstractEntity {

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
  private QuoteReplyReviewStateEnum reviewState = QuoteReplyReviewStateEnum.APPROVED;

  /**
   * comment.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private CommentEntity comment;

  /**
   * quote replies.
   */
  @OneToMany(
      mappedBy = "quoteReply",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<QuoteReplyEntity> quoteReplies = new HashSet<>();

  /**
   * quote reply.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private QuoteReplyEntity quoteReply;

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
   * unique identifier.
   */
  private String uniqueIdentifier;

  /**
   * get content and id.
   *
   * @return String
   */
  public String getContentAndId() {
    return getContent() + "#" + getId();
  }
}