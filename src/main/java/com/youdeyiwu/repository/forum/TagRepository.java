package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.TagEntity;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * tag.
 *
 * @author dafengzhen
 */
public interface TagRepository extends JpaRepositoryImplementation<TagEntity, Long> {

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
