package com.youdeyiwu.model.vo;

import java.io.Serializable;
import java.time.OffsetDateTime;
import java.util.Objects;
import lombok.Getter;
import lombok.Setter;

/**
 * abstract entity.
 *
 * @author dafengzhen
 */
@Getter
@Setter
public abstract class AbstractEntityVo implements Serializable {

  /**
   * id.
   */
  private Long id;

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

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (!(o instanceof AbstractEntityVo that)) {
      return false;
    }
    return Objects.equals(id, that.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id);
  }
}
