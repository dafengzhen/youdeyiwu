package com.youdeyiwu.model.entity;

import jakarta.persistence.Column;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.Version;
import java.io.Serializable;
import java.time.OffsetDateTime;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

/**
 * id less abstract entity.
 *
 * @author dafengzhen
 */
@Getter
@Setter
@EntityListeners(AuditingEntityListener.class)
@MappedSuperclass
public abstract class IdLessAbstractEntity implements Serializable {

  /**
   * createdBy.
   */
  @CreatedBy
  private Long createdBy;

  /**
   * updatedBy.
   */
  @LastModifiedBy
  private Long updatedBy;

  /**
   * createdOn.
   */
  @CreatedDate
  @Column(nullable = false)
  private OffsetDateTime createdOn;

  /**
   * updatedOn.
   */
  @LastModifiedDate
  private OffsetDateTime updatedOn;

  /**
   * deleted.
   */
  @Column(nullable = false)
  private Boolean deleted = false;

  /**
   * version.
   */
  @Version
  private Short version = 0;

}
