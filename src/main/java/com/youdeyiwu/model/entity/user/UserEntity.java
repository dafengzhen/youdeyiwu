package com.youdeyiwu.model.entity.user;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostFavoriteEntity;
import com.youdeyiwu.model.entity.forum.PostUserEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.message.GlobalMessageUserEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.point.PointEntity;
import com.youdeyiwu.model.entity.point.PointHistoryEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Transient;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

/**
 * user.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class UserEntity extends AbstractEntity implements UserDetails {

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
  @Column(unique = true)
  private String username;

  /**
   * password.
   */
  private String password;

  /**
   * email.
   */
  @Column(unique = true)
  private String email;

  /**
   * token.
   */
  @Column(unique = true)
  private String token;

  /**
   * last login time.
   */
  @Column(nullable = false)
  private LocalDateTime lastLoginTime = LocalDateTime.now();

  /**
   * root refers to whether someone is a forum administrator.
   * There may be multiple forum administrators, but typically there is only one.
   */
  private Boolean root = false;

  /**
   * account not expired.
   */
  @Getter(AccessLevel.NONE)
  @Column(nullable = false)
  private Boolean accountNonExpired = true;

  /**
   * credentials not expired.
   */
  @Getter(AccessLevel.NONE)
  @Column(nullable = false)
  private Boolean credentialsNonExpired = true;

  /**
   * account not locked.
   */
  @Getter(AccessLevel.NONE)
  @Column(nullable = false)
  private Boolean accountNonLocked = true;

  /**
   * enabled.
   */
  @Getter(AccessLevel.NONE)
  @Column(nullable = false)
  private Boolean enabled = true;

  /**
   * authorities.
   */
  @Transient
  private Collection<? extends GrantedAuthority> authorities = new HashSet<>();

  /**
   * roles.
   */
  @ManyToMany(cascade = {
      CascadeType.DETACH,
      CascadeType.MERGE,
      CascadeType.PERSIST,
      CascadeType.REFRESH
  })
  @JsonIgnore
  @ToString.Exclude
  private Set<RoleEntity> roles = new HashSet<>();

  /**
   * sections.
   */
  @ManyToMany(mappedBy = "admins")
  @JsonIgnore
  @ToString.Exclude
  private Set<SectionEntity> sections = new HashSet<>();

  /**
   * section allows.
   */
  @ManyToMany(mappedBy = "allows")
  @JsonIgnore
  @ToString.Exclude
  private Set<SectionEntity> sectionAllows = new HashSet<>();

  /**
   * section blocks.
   */
  @ManyToMany(mappedBy = "blocks")
  @JsonIgnore
  @ToString.Exclude
  private Set<SectionEntity> sectionBlocks = new HashSet<>();

  /**
   * user posts.
   */
  @OneToMany(
      mappedBy = "user",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<PostUserEntity> userPosts = new HashSet<>();

  /**
   * post allows.
   */
  @ManyToMany(mappedBy = "allows")
  @JsonIgnore
  @ToString.Exclude
  private Set<SectionEntity> postAllows = new HashSet<>();

  /**
   * post blocks.
   */
  @ManyToMany(mappedBy = "blocks")
  @JsonIgnore
  @ToString.Exclude
  private Set<SectionEntity> postBlocks = new HashSet<>();

  /**
   * postFavorites.
   */
  @OneToMany(
      mappedBy = "user",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<PostFavoriteEntity> postFavorites = new HashSet<>();

  /**
   * comments.
   */
  @OneToMany(
      mappedBy = "user",
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
      mappedBy = "user",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<QuoteReplyEntity> quoteReplies = new HashSet<>();

  /**
   * sendMessages.
   */
  @OneToMany(
      mappedBy = "sender",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<MessageEntity> sendMessages = new HashSet<>();

  /**
   * receivedMessages.
   */
  @OneToMany(
      mappedBy = "receiver",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<MessageEntity> receivedMessages = new HashSet<>();

  /**
   * user global messages.
   */
  @OneToMany(
      mappedBy = "user",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<GlobalMessageUserEntity> userGlobalMessages = new HashSet<>();

  /**
   * point.
   */
  @OneToOne
  @JsonIgnore
  @ToString.Exclude
  private PointEntity point;

  /**
   * point histories.
   */
  @OneToMany(
      mappedBy = "user",
      cascade = CascadeType.ALL,
      orphanRemoval = true
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<PointHistoryEntity> pointHistories = new HashSet<>();

  @Override
  public Collection<? extends GrantedAuthority> getAuthorities() {
    return authorities;
  }

  @Override
  public boolean isAccountNonExpired() {
    return accountNonExpired;
  }

  @Override
  public boolean isAccountNonLocked() {
    return accountNonLocked;
  }

  @Override
  public boolean isCredentialsNonExpired() {
    return credentialsNonExpired;
  }

  @Override
  public boolean isEnabled() {
    return enabled;
  }

}