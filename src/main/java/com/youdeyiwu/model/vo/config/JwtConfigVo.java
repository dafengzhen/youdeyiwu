package com.youdeyiwu.model.vo.config;

import java.io.Serializable;
import lombok.Data;

/**
 * jwt config.
 *
 * @author dafengzhen
 */
@Data
public class JwtConfigVo implements Serializable {

  /**
   * secret.
   */
  private String secret;

}