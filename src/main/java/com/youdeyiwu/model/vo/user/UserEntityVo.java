package com.youdeyiwu.model.vo.user;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.PostFavoriteEntityVo;
import com.youdeyiwu.model.vo.forum.SectionEntityVo;
import com.youdeyiwu.model.vo.forum.TagEntityVo;
import java.time.OffsetDateTime;
import java.util.Set;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * user.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class UserEntityVo extends AbstractEntityVo {

  /**
   * alias.
   */
  private String alias;

  /**
   * avatar.
   */
  private String avatar;

  /**
   * one sentence.
   */
  private String oneSentence;

  /**
   * username.
   */
  private String username;

  /**
   * email.
   */
  private String email;

  /**
   * temporary storage (store post content temporarily).
   */
  private String temporaryStorage;

  /**
   * last login time.
   */
  private OffsetDateTime lastLoginTime;

  /**
   * root.
   */
  private Boolean root;

  /**
   * no posting allowed (Here refers to creating a post).
   */
  private Boolean noPostingAllowed;

  /**
   * disable comments.
   */
  private Boolean disableComments;

  /**
   * disable replies.
   */
  private Boolean disableReplies;

  /**
   * no posting reason.
   */
  private String noPostingReason;

  /**
   * comment disable reason.
   */
  private String commentDisableReason;

  /**
   * reply disable reason.
   */
  private String replyDisableReason;

  /**
   * account not expired.
   */
  private Boolean accountNonExpired;

  /**
   * credentials not expired.
   */
  private Boolean credentialsNonExpired;

  /**
   * account not locked.
   */
  private Boolean accountNonLocked;

  /**
   * enabled.
   */
  private Boolean enabled;

  /**
   * roles.
   */
  private Set<RoleEntityVo> roles;

  /**
   * posts.
   */
  private Set<PostEntityVo> posts;

  /**
   * favorites.
   */
  private Set<PostFavoriteEntityVo> favorites;

  /**
   * related sections.
   */
  private Set<SectionEntityVo> relatedSections;

  /**
   * related tags.
   */
  private Set<TagEntityVo> relatedTags;

  /**
   * related statistics.
   */
  private UserStatisticsVo relatedStatistics;

  /**
   * sections.
   */
  private Set<SectionEntityVo> sections;

}