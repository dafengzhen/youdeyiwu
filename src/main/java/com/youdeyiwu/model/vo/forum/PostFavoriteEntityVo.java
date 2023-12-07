package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * post favorite.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PostFavoriteEntityVo extends AbstractEntityVo {

  /**
   * name.
   */
  private String name;

  /**
   * overview.
   */
  private String overview;

  /**
   * content.
   */
  private String content;

  /**
   * content link.
   */
  private String contentLink;

  /**
   * post id.
   */
  private Long postId;

}