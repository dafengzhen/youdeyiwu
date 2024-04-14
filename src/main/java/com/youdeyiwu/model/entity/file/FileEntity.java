package com.youdeyiwu.model.entity.file;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.youdeyiwu.enums.file.BusinessTypeEnum;
import com.youdeyiwu.enums.file.FileCategoryEnum;
import com.youdeyiwu.enums.file.StorageServiceTypeEnum;
import com.youdeyiwu.model.entity.AbstractEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
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
 * file.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@DynamicInsert
@DynamicUpdate
@Entity
public class FileEntity extends AbstractEntity {

  /**
   * url.
   */
  private String url;

  /**
   * name.
   */
  private String name;

  /**
   * original name.
   */
  private String originalName;

  /**
   * overview.
   */
  private String overview;

  /**
   * cover image.
   */
  @Lob
  @Basic(fetch = FetchType.LAZY)
  @Column(columnDefinition = "mediumblob")
  @JsonIgnore
  @ToString.Exclude
  private byte[] file;

  /**
   * file category.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private FileCategoryEnum fileCategory = FileCategoryEnum.IMAGE;

  /**
   * storage service type.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private StorageServiceTypeEnum storageServiceType = StorageServiceTypeEnum.DB;

  /**
   * business type.
   */
  @Enumerated
  @Column(columnDefinition = "smallint", nullable = false)
  private BusinessTypeEnum businessType = BusinessTypeEnum.USER;

  /**
   * content type.
   */
  private String contentType;

  /**
   * media type.
   */
  private String mediaType;

  /**
   * size.
   */
  private Long size;

  /**
   * bucket name.
   */
  private String bucketName;

  /**
   * object name.
   */
  private String objectName;

  /**
   * view count.
   */
  private Integer viewCount = 0;

  /**
   * digest.
   */
  @Column(unique = true)
  private String digest;

  /**
   * object key.
   */
  @Column(unique = true)
  private String objectKey;

  /**
   * object value.
   */
  @Basic(fetch = FetchType.LAZY)
  @Column(columnDefinition = "text")
  @JsonIgnore
  @ToString.Exclude
  private String objectValue;

  /**
   * user.
   */
  @ManyToOne
  @JsonIgnore
  @ToString.Exclude
  private UserEntity user;

}