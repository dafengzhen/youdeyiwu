package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreatePostDto;
import com.youdeyiwu.model.dto.forum.QueryParamsPostDto;
import com.youdeyiwu.model.dto.forum.UpdatePostDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsPostDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.CommentReplyVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import java.util.Set;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

/**
 * post.
 *
 * @author dafengzhen
 */
public interface PostService {

  /**
   * create.
   *
   * @param dto dto
   * @return PostEntity
   */
  PostEntity create(CreatePostDto dto);

  /**
   * view page.
   *
   * @param id id
   */
  void viewPage(Long id);

  /**
   * upload cover.
   *
   * @param id   id
   * @param file file
   */
  void uploadCover(Long id, MultipartFile file);

  /**
   * update like.
   *
   * @param id id
   */
  void updateLike(Long id);

  /**
   * update favorite.
   *
   * @param id id
   */
  void updateFavorite(Long id);

  /**
   * update tags.
   *
   * @param id  id
   * @param dto dto
   */
  void updateTags(Long id, UpdateTagsPostDto dto);

  /**
   * update.
   *
   * @param id  id
   * @param dto dto
   */
  void update(Long id, UpdatePostDto dto);

  /**
   * query random.
   *
   * @return List
   */
  Set<PostEntityVo> queryRandom();

  /**
   * select all.
   *
   * @param pageable pageable
   * @param dto      dto
   * @return PageVo
   */
  PageVo<PostEntityVo> selectAll(Pageable pageable, QueryParamsPostDto dto);

  /**
   * query comment reply.
   *
   * @param pageable pageable
   * @param id       id
   * @return PageVo
   */
  PageVo<CommentReplyVo> queryCommentReply(Pageable pageable, Long id);

  /**
   * query details.
   *
   * @param pageable pageable
   * @param id       id
   * @return PostEntityVo
   */
  PostEntityVo queryDetails(Pageable pageable, Long id);

  /**
   * query cover.
   *
   * @param id id
   * @return byte[]
   */
  byte[] queryCover(Long id);

  /**
   * query.
   *
   * @param id id
   * @return PostEntityVo
   */
  PostEntityVo query(Long id);

  /**
   * queryAll.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<PostEntityVo> queryAll(Pageable pageable);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}