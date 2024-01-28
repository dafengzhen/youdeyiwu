package com.youdeyiwu.model.vo.point;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * point.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PointEntityVo extends AbstractEntityVo {

  /**
   * points.
   */
  private Integer points;

  /**
   * min points.
   */
  private Integer minPoints;

  /**
   * max points.
   */
  private Integer maxPoints;

  /**
   * user.
   */
  private UserEntityVo user;

}