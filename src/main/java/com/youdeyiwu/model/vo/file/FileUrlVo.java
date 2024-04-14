package com.youdeyiwu.model.vo.file;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.io.Serializable;
import lombok.Data;

/**
 * url.
 *
 * @author dafengzhen
 */
@Data
public class FileUrlVo implements Serializable {

  /**
   * default url.
   */
  @JsonProperty("default")
  private String defaultUrl;

}

