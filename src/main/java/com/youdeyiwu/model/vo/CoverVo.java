package com.youdeyiwu.model.vo;

import com.youdeyiwu.enums.file.FileTypeEnum;
import java.io.Serializable;
import lombok.Data;

/**
 * cover.
 *
 * @author dafengzhen
 */
@Data
public class CoverVo implements Serializable {

  /**
   * cover image.
   */
  private byte[] coverImage;

  /**
   * cover image type.
   */
  private FileTypeEnum coverImageType;

}
