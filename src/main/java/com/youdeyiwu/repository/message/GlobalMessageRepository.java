package com.youdeyiwu.repository.message;

import com.youdeyiwu.model.entity.message.GlobalMessageEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * global message.
 *
 * @author dafengzhen
 */
public interface GlobalMessageRepository extends CrudRepository<GlobalMessageEntity, Long>,
    PagingAndSortingRepository<GlobalMessageEntity, Long>, CustomizedMessageRepository {

  /**
   * findAllBySenderIsNull.
   *
   * @param pageable pageable
   * @return Page
   */
  Page<GlobalMessageEntity> findAllBySenderIsNull(Pageable pageable);
}
