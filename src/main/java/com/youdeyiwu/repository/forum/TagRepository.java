package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.TagEntity;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.PagingAndSortingRepository;

/**
 * tag.
 *
 * @author dafengzhen
 */
public interface TagRepository extends CrudRepository<TagEntity, Long>,
    PagingAndSortingRepository<TagEntity, Long> {

  /**
   * existsByName.
   *
   * @param name name
   * @return boolean
   */
  boolean existsByName(String name);

  /**
   * findByName.
   *
   * @param name name
   * @return TagEntity
   */
  TagEntity findByName(String name);

}
