package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreateCommentDto;
import com.youdeyiwu.model.dto.forum.UpdateStateCommentDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.vo.forum.CommentEntityVo;

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

  /**
   * update state.
   *
   * @param id  id
   * @param dto dto
   */
  void updateState(Long id, UpdateStateCommentDto dto);

  /**
   * update like.
   *
   * @param id id
   */
  void updateLike(Long id);

  /**
   * query.
   *
   * @param id id
   * @return CommentEntityVo
   */
  CommentEntityVo query(Long id);
}