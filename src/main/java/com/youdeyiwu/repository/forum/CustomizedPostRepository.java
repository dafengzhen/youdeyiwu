package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.dto.forum.QueryParamsPostDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.vo.forum.CommentReplyEntityVo;
import java.util.List;
import org.springframework.data.domain.Page;

/**
 * post.
 *
 * @author dafengzhen
 */
public interface CustomizedPostRepository {

  /**
   * find random posts.
   *
   * @return List
   */
  List<PostEntity> findRandomPosts();

  /**
   * find all.
   *
   * @param position    position
   * @param dto         dto
   * @param isAnonymous isAnonymous
   * @param userId      userId
   * @return Page
   */
  Page<PostEntity> findAll(
      PaginationPositionDto position,
      QueryParamsPostDto dto,
      Boolean isAnonymous,
      Long userId
  );

  /**
   * find all comment reply.
   *
   * @param position position
   * @param postId   postId
   * @return Page
   */
  Page<CommentReplyEntityVo> findAllCommentReply(
      PaginationPositionDto position,
      Long postId
  );
}
