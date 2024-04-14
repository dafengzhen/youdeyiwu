package com.youdeyiwu.model.vo.file;

import com.youdeyiwu.enums.file.BusinessTypeEnum;
import com.youdeyiwu.enums.file.FileCategoryEnum;
import com.youdeyiwu.enums.file.StorageServiceTypeEnum;
import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import jakarta.persistence.Column;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * file.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class FileEntityVo extends AbstractEntityVo {

  /**
   * url.
   */
  private String url;

  /**
   * urls.
   */
  private FileUrlsVo urls;

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
   * file category.
   */
  private FileCategoryEnum fileCategory;

  /**
   * storage service type.
   */
  private StorageServiceTypeEnum storageServiceType;

  /**
   * business type.
   */
  private BusinessTypeEnum businessType;

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
  private Integer viewCount;

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
   * user.
   */
  private UserEntityVo user;

}