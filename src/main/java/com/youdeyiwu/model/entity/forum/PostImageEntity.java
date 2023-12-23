package com.youdeyiwu.model.entity.forum;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.file.FileTypeEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import jakarta.persistence.Basic;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.Lob;
import jakarta.persistence.ManyToOne;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DynamicInsert;
import org.hibernate.annotations.DynamicUpdate;

/**
 * post image.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class PostImageEntity extends AbstractEntity {

  /**
   * url.
   */
  @Column(nullable = false)
  private String url;

  /**
   * image.
   */
  @Lob
  @Basic(fetch = FetchType.LAZY)
  @Column(columnDefinition = "mediumblob")
  @JsonIgnore
  @ToString.Exclude
  private byte[] image;

  /**
   * image type.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private FileTypeEnum imageType = FileTypeEnum.JPG;

  /**
   * sort.
   */
  @Column(nullable = false)
  private Integer sort = 0;

  /**
   * post.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private PostEntity post;

}