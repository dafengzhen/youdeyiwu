package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.TagGroupEntity;
import java.util.Optional;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * tag group.
 *
 * @author dafengzhen
 */
public interface TagGroupRepository extends JpaRepositoryImplementation<TagGroupEntity, Long> {

  /**
   * findByName.
   *
   * @param name name
   * @return Optional
   */
  Optional<TagGroupEntity> findByName(String name);
}
