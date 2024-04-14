package com.youdeyiwu.repository.file;

import com.youdeyiwu.model.entity.file.FileEntity;
import java.util.Optional;
import org.springframework.data.jpa.repository.support.JpaRepositoryImplementation;

/**
 * file.
 *
 * @author dafengzhen
 */
public interface FileRepository extends JpaRepositoryImplementation<FileEntity, Long> {

  /**
   * findByDigest.
   *
   * @param digest digest
   * @return Optional
   */
  Optional<FileEntity> findByDigest(String digest);
}
