package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.file.FileTypeEnum;
import com.youdeyiwu.enums.forum.SectionStateEnum;
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
import jakarta.persistence.OneToMany;
import java.util.HashSet;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * section.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class SectionEntity extends AbstractEntity {

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
   * sort.
   */
  @Column(nullable = false)
  private Integer sort = 0;

  /**
   * states.
   */
  @Enumerated
  @ElementCollection(fetch = FetchType.EAGER)
  private Set<SectionStateEnum> states = new HashSet<>();

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
   * sectionGroups.
   */
  @ManyToMany(mappedBy = "sections")
  @JsonIgnore
  @ToString.Exclude
  private Set<SectionGroupEntity> sectionGroups = new HashSet<>();

  /**
   * admins.
   */
  @ManyToMany(cascade = {
      CascadeType.DETACH,
      CascadeType.MERGE,
      CascadeType.PERSIST,
      CascadeType.REFRESH
  })
  @JsonIgnore
  @ToString.Exclude
  private Set<UserEntity> admins = new HashSet<>();

  /**
   * tagGroups.
   */
  @ManyToMany(cascade = {
      CascadeType.DETACH,
      CascadeType.MERGE,
      CascadeType.PERSIST,
      CascadeType.REFRESH
  })
  @JsonIgnore
  @ToString.Exclude
  private Set<TagGroupEntity> tagGroups = new HashSet<>();

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
   * posts.
   */
  @OneToMany(
      mappedBy = "section",
      cascade = CascadeType.ALL
  )
  @JsonIgnore
  @ToString.Exclude
  private Set<PostEntity> posts = new HashSet<>();

}