package com.youdeyiwu.model.dto;

import java.io.Serializable;
import org.springframework.data.domain.Pageable;

/**
 * pagination position.
 *
 * @param pageable    pageable
 * @param firstResult firstResult
 * @param maxResults  maxResults
 */
public record PaginationPositionDto(
    Pageable pageable,

    Integer firstResult,

    Integer maxResults
) implements Serializable {

  /**
   * pagination position.
   *
   * @param pageable pageable
   */
  public PaginationPositionDto(Pageable pageable) {
    this(pageable, (int) pageable.getOffset(), pageable.getPageSize());
  }
}
