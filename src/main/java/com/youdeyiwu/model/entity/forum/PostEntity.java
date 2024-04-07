package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.file.FileTypeEnum;
import com.youdeyiwu.enums.forum.PostReviewStateEnum;
import com.youdeyiwu.enums.forum.PostSortStateEnum;
import com.youdeyiwu.enums.forum.PostStateEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import jakarta.persistence.Basic;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Lob;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Transient;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * post.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PostEntity extends AbstractEntity {

  /**
   * name.
   */
  @Column(nullable = false)
  private String name;

  /**
   * cover.
   */
  private String cover;

  /**
   * cover image.
   */
  @Lob
  @Basic(fetch = FetchType.LAZY)
  @Column(columnDefinition = "mediumblob")
  @JsonIgnore
  @ToString.Exclude
  private byte[] coverImage;

  /**
   * cover image type.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private FileTypeEnum coverImageType = FileTypeEnum.JPG;

  /**
   * overview.
   */
  private String overview;

  /**
   * content.
   */
  @Column(columnDefinition = "text")
  private String content;

  /**
   * content link.
   */
  private String contentLink;

  /**
   * disable comments.
   */
  private Boolean disableComments = false;

  /**
   * disable replies.
   */
  private Boolean disableReplies = false;

  /**
   * badges.
   */
  @OneToMany(
      mappedBy = "post",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<PostBadgeEntity> badges = new HashSet<>();

  /**
   * images.
   */
  @OneToMany(
      mappedBy = "post",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<PostImageEntity> images = new HashSet<>();

  /**
   * states.
   */
  @Enumerated
  @ElementCollection(fetch = FetchType.EAGER)
  private Set<PostStateEnum> states = EnumSet.of(PostStateEnum.SHOW);

  /**
   * review state.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private PostReviewStateEnum reviewState = PostReviewStateEnum.APPROVED;

  /**
   * sort state.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private PostSortStateEnum sortState = PostSortStateEnum.DEFAULT;

  /**
   * allows.
   */
  @ManyToMany(cascade = {
      CascadeType.DETACH,
      CascadeType.MERGE,
      CascadeType.PERSIST,
      CascadeType.REFRESH
  })
  @JsonIgnore
  @ToString.Exclude
  private Set<UserEntity> allows = new HashSet<>();

  /**
   * blocks.
   */
  @ManyToMany(cascade = {
      CascadeType.DETACH,
      CascadeType.MERGE,
      CascadeType.PERSIST,
      CascadeType.REFRESH
  })
  @JsonIgnore
  @ToString.Exclude
  private Set<UserEntity> blocks = new HashSet<>();

  /**
   * access key.
   */
  private String accessKey;

  /**
   * page views.
   */
  @Column(nullable = false)
  private Long pageViews = 0L;

  /**
   * comments count.
   */
  @Column(nullable = false)
  private Long commentsCount = 0L;

  /**
   * replies count.
   */
  @Column(nullable = false)
  private Long repliesCount = 0L;

  /**
   * likes count.
   */
  @Column(nullable = false)
  private Long likesCount = 0L;

  /**
   * followers count.
   */
  @Column(nullable = false)
  private Long followersCount = 0L;

  /**
   * favorites count.
   */
  @Column(nullable = false)
  private Long favoritesCount = 0L;

  /**
   * initial score.
   */
  @Column(nullable = false)
  private Long initialScore = 100L;

  /**
   * section.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private SectionEntity section;

  /**
   * histories.
   */
  @OneToMany(
      mappedBy = "post",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<PostHistoryEntity> histories = new HashSet<>();

  /**
   * tags.
   */
  @ManyToMany(cascade = {
      CascadeType.DETACH,
      CascadeType.MERGE,
      CascadeType.PERSIST,
      CascadeType.REFRESH
  })
  @JsonIgnore
  @ToString.Exclude
  private Set<TagEntity> tags = new HashSet<>();

  /**
   * user.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity user;

  /**
   * comments.
   */
  @OneToMany(
      mappedBy = "post",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<CommentEntity> comments = new HashSet<>();

  /**
   * quoteReplies.
   */
  @OneToMany(
      mappedBy = "post",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<QuoteReplyEntity> quoteReplies = new HashSet<>();

  /**
   * post users.
   */
  @OneToMany(
      mappedBy = "post",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<PostUserEntity> postUsers = new HashSet<>();

  /**
   * post review queue.
   */
  @OneToOne
  @JsonIgnore
  @ToString.Exclude
  private PostReviewQueueEntity postReviewQueue;

  /**
   * review state.
   */
  @Transient
  private PostReviewStateEnum oldReviewState;

  /**
   * transient review reason.
   */
  @Transient
  private String reviewReason;

  /**
   * disable comments.
   */
  @Transient
  private Boolean oldDisableComments;

  /**
   * disable replies.
   */
  @Transient
  private Boolean oldDisableReplies;

  /**
   * get name and id.
   *
   * @return String
   */
  public String getNameAndId() {
    return getName() + "#" + getId();
  }

  /**
   * get link.
   *
   * @return String
   */
  public String getLink() {
    return "/posts/" + getId();
  }
}