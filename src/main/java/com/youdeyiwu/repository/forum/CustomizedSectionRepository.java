package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import java.util.List;

/**
 * section.
 *
 * @author dafengzhen
 */
public interface CustomizedSectionRepository {

  /**
   * find all.
   *
   * @param accessKey   accessKey
   * @param isAnonymous isAnonymous
   * @param user        user
   * @param root        root
   * @return Page
   */
  List<SectionEntity> findAll(
      String accessKey,
      Boolean isAnonymous,
      UserEntity user,
      UserEntity root
  );
}
