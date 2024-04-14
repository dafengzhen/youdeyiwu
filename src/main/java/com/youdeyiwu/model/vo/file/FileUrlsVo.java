package com.youdeyiwu.model.vo.file;

import java.io.Serializable;
import lombok.Data;

/**
 * urls.
 *
 * @author dafengzhen
 */
@Data
public class FileUrlsVo implements Serializable {

  /**
   * url.
   */
  private String url;

  /**
   * urls.
   */
  private FileUrlVo urls;

}

