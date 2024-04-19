package com.youdeyiwu.constant;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

/**
 * root config.
 *
 * @author dafengzhen
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class RootConfigConstant {

  /**
   * secret.
   */
  public static final String SECRET = "secret";

  /**
   * disable registration.
   */
  public static final String DISABLE_REGISTRATION = "disableRegistration";

  /**
   * disable anonymous posts.
   */
  public static final String DISABLE_ANONYMOUS_POSTS = "disableAnonymousPosts";

  /**
   * disable anonymous comments.
   */
  public static final String DISABLE_ANONYMOUS_COMMENTS = "disableAnonymousComments";

  /**
   * disable anonymous replies.
   */
  public static final String DISABLE_ANONYMOUS_REPLIES = "disableAnonymousReplies";

}