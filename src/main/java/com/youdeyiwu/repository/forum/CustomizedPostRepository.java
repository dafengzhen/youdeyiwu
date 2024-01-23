package com.youdeyiwu.repository.forum;

import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.dto.forum.QueryParamsPost;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
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
   * @param accessKey   accessKey
   * @param isAnonymous isAnonymous
   * @param user        user
   * @param root        root
   * @return Page
   */
  Page<PostEntity> findAll(
      PaginationPositionDto position,
      QueryParamsPost dto,
      String accessKey,
      Boolean isAnonymous,
      UserEntity user,
      UserEntity root
  );

  /**
   * find all comment reply.
   *
   * @param position    position
   * @param postEntity  postEntity
   * @param isAnonymous isAnonymous
   * @param user        user
   * @param root        root
   * @return Page
   */
  Page<CommentReplyEntityVo> findAllCommentReply(
      PaginationPositionDto position,
      PostEntity postEntity,
      Boolean isAnonymous,
      UserEntity user,
      UserEntity root
  );

  /**
   * find post review queues.
   *
   * @param position    position
   * @param isAnonymous isAnonymous
   * @param user        user
   * @param root        root
   * @return Page
   */
  Page<PostEntity> findPostReviewQueues(
      PaginationPositionDto position,
      Boolean isAnonymous,
      UserEntity user,
      UserEntity root
  );
}
