package com.youdeyiwu.model.vo;

import java.io.Serializable;
import java.time.OffsetDateTime;
import lombok.Getter;
import lombok.Setter;

/**
 * id less abstract entity.
 *
 * @author dafengzhen
 */
@Getter
@Setter
public abstract class IdLessAbstractEntityVo implements Serializable {

  /**
   * createdBy.
   */
  private Long createdBy;

  /**
   * updatedBy.
   */
  private Long updatedBy;

  /**
   * createdOn.
   */
  private OffsetDateTime createdOn;

  /**
   * updatedOn.
   */
  private OffsetDateTime updatedOn;

  /**
   * deleted.
   */
  private Boolean deleted;

}
