package com.youdeyiwu.model.vo.config;

import java.io.Serializable;
import lombok.Data;

/**
 * point config.
 *
 * @author dafengzhen
 */
@Data
public class PointConfigVo implements Serializable {

  /**
   * enable.
   */
  private Boolean enable;

  /**
   * init points.
   */
  private Integer initPoints;

}