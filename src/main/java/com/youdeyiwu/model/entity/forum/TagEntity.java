package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.ManyToMany;
import java.util.HashSet;
import java.util.Set;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * tag.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class TagEntity extends AbstractEntity {

  /**
   * name.
   */
  @Column(unique = true, nullable = false)
  private String name;

  /**
   * sort.
   */
  @Column(nullable = false)
  private Integer sort = 0;

  /**
   * tagGroups.
   */
  @ManyToMany(mappedBy = "tags")
  @JsonIgnore
  @ToString.Exclude
  private Set<TagGroupEntity> tagGroups = new HashSet<>();

  /**
   * sections.
   */
  @ManyToMany(mappedBy = "tags")
  @JsonIgnore
  @ToString.Exclude
  private Set<SectionEntity> sections = new HashSet<>();

  /**
   * posts.
   */
  @ManyToMany(mappedBy = "tags")
  @JsonIgnore
  @ToString.Exclude
  private Set<SectionEntity> posts = new HashSet<>();

}