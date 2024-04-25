package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.SectionEntity;
import java.util.Optional;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * section.
 *
 * @author dafengzhen
 */
public interface SectionRepository
    extends JpaRepositoryImplementation<SectionEntity, Long>, CustomizedSectionRepository {

  /**
   * findByName.
   *
   * @param name name
   * @return Optional
   */
  Optional<SectionEntity> findByName(String name);
}
