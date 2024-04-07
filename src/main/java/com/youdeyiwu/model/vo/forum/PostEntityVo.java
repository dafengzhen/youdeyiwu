package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.enums.file.FileTypeEnum;
import com.youdeyiwu.enums.forum.PostReviewStateEnum;
import com.youdeyiwu.enums.forum.PostSortStateEnum;
import com.youdeyiwu.enums.forum.PostStateEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * post.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PostEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * cover.
   */
  private String cover;

  /**
   * cover image.
   */
  private byte[] coverImage;

  /**
   * cover image type.
   */
  private FileTypeEnum coverImageType;

  /**
   * overview.
   */
  private String overview;

  /**
   * content.
   */
  private String content;

  /**
   * content link.
   */
  private String contentLink;

  /**
   * disable comments.
   */
  private Boolean disableComments;

  /**
   * disable replies.
   */
  private Boolean disableReplies;

  /**
   * badges.
   */
  private Set<PostBadgeEntityVo> badges;

  /**
   * images.
   */
  private Set<PostImageEntityVo> images;

  /**
   * states.
   */
  private Set<PostStateEnum> states;

  /**
   * reviewState.
   */
  private PostReviewStateEnum reviewState;

  /**
   * sortState.
   */
  private PostSortStateEnum sortState;

  /**
   * allows.
   */
  private Set<UserEntityVo> allows;

  /**
   * blocks.
   */
  private Set<UserEntityVo> blocks;

  /**
   * access key.
   */
  private String accessKey;

  /**
   * page views.
   */
  private Long pageViews;

  /**
   * comments count.
   */
  private Long commentsCount;

  /**
   * replies count.
   */
  private Long repliesCount;

  /**
   * likes count.
   */
  private Long likesCount;

  /**
   * followers count.
   */
  private Long followersCount;

  /**
   * favorites count.
   */
  private Long favoritesCount;

  /**
   * section.
   */
  private SectionEntityVo section;

  /**
   * tags.
   */
  private Set<TagEntityVo> tags;

  /**
   * user.
   */
  private UserEntityVo user;

  /**
   * liked.
   */
  private Boolean liked;

  /**
   * followed.
   */
  private Boolean followed;

  /**
   * favorited.
   */
  private Boolean favorited;

  /**
   * comments.
   */
  private PageVo<CommentReplyVo> comments;

  /**
   * post review queue.
   */
  private PostReviewQueueEntityVo postReviewQueue;

}