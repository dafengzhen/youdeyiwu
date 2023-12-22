package com.youdeyiwu.model.vo;

import com.youdeyiwu.enums.file.FileTypeEnum;
import lombok.Data;

/**
 * cover.
 *
 * @author dafengzhen
 */
@Data
public class CoverVo {

  /**
   * cover image.
   */
  private byte[] coverImage;

  /**
   * cover image type.
   */
  private FileTypeEnum coverImageType;

}
