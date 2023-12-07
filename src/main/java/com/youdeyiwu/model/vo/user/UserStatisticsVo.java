package com.youdeyiwu.model.vo.user;

import java.io.Serializable;
import lombok.Data;

/**
 * user statistics.
 *
 * @author dafengzhen
 */
@Data
public class UserStatisticsVo implements Serializable {

  /**
   * sections.
   */
  private Integer sections;

  /**
   * tags.
   */
  private Integer tags;

  /**
   * posts.
   */
  private Integer posts;

  /**
   * comments.
   */
  private Integer comments;

  /**
   * replies.
   */
  private Integer replies;

  /**
   * views.
   */
  private Integer views;

}

