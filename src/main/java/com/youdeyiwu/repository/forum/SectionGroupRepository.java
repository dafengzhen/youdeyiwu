package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.SectionGroupEntity;
import java.util.Optional;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * section group.
 *
 * @author dafengzhen
 */
public interface SectionGroupRepository
    extends JpaRepositoryImplementation<SectionGroupEntity, Long> {

  /**
   * findByName.
   *
   * @param name name
   * @return Optional
   */
  Optional<SectionGroupEntity> findByName(String name);
}
