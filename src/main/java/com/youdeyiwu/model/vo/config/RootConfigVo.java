package com.youdeyiwu.model.vo.config;

import java.io.Serializable;
import lombok.Data;

/**
 * root config.
 *
 * @author dafengzhen
 */
@Data
public class RootConfigVo implements Serializable {

  /**
   * disable registration.
   */
  private Boolean disableRegistration;

  /**
   * disable anonymous posts.
   */
  private Boolean disableAnonymousPosts;

  /**
   * disable anonymous comments.
   */
  private Boolean disableAnonymousComments;

  /**
   * disable anonymous replies.
   */
  private Boolean disableAnonymousReplies;

}