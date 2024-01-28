package com.youdeyiwu.model.vo.point;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * point history.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PointHistoryEntityVo extends AbstractEntityVo {

  /**
   * point Value.
   */
  private Integer pointValue;

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
   * reason.
   */
  private String reason;

  /**
   * user.
   */
  private UserEntityVo user;

}