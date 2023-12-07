package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreateCommentDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;

/**
 * comment.
 *
 * @author dafengzhen
 */
public interface CommentService {

  /**
   * create.
   *
   * @param dto dto
   * @return CommentEntity
   */
  CommentEntity create(CreateCommentDto dto);
}