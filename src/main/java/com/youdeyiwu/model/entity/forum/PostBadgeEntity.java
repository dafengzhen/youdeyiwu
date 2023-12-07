package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.ManyToOne;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * post badge.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PostBadgeEntity extends AbstractEntity {

  /**
   * name.
   */
  @Column(nullable = false)
  private String name;

  /**
   * sort.
   */
  @Column(nullable = false)
  private Integer sort = 0;

  /**
   * styles.
   */
  private String styles;

  /**
   * classes.
   */
  private String classes;

  /**
   * post.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private PostEntity post;

}