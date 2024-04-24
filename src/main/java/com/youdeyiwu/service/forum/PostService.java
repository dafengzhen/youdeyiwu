package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.CreatePostDto;
import com.youdeyiwu.model.dto.forum.DisableCommentReplyPostDto;
import com.youdeyiwu.model.dto.forum.DisableUserCommentReplyPostDto;
import com.youdeyiwu.model.dto.forum.QueryParamsPostDto;
import com.youdeyiwu.model.dto.forum.UpdatePostDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionPostDto;
import com.youdeyiwu.model.dto.forum.UpdateStatesPostDto;
import com.youdeyiwu.model.dto.forum.UpdateStylesPostDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsPostDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.vo.CoverVo;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.CommentReplyVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.PostUserEntityVo;
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
   * check disable anonymous posts.
   */
  void checkDisableAnonymousPosts();

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
   * @param ip ip
   */
  void viewPage(Long id, String ip);

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
   * disable comment reply.
   *
   * @param id  id
   * @param dto dto
   */
  void disableCommentReply(Long id, DisableCommentReplyPostDto dto);

  /**
   * disable user comment reply.
   *
   * @param id     id
   * @param userId userId
   * @param dto    dto
   */
  void disableUserCommentReply(Long id, Long userId, DisableUserCommentReplyPostDto dto);

  /**
   * update styles.
   *
   * @param id  id
   * @param dto dto
   */
  void updateStyles(Long id, UpdateStylesPostDto dto);

  /**
   * update section.
   *
   * @param id  id
   * @param dto dto
   */
  void updateSection(Long id, UpdateSectionPostDto dto);

  /**
   * update states.
   *
   * @param id  id
   * @param dto dto
   */
  void updateStates(Long id, UpdateStatesPostDto dto);

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
   * @param postKey  postKey
   * @return PageVo
   */
  PageVo<PostEntityVo> selectAll(
      Pageable pageable,
      QueryParamsPostDto dto,
      String postKey
  );

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
   * @param postKey  postKey
   * @return PostEntityVo
   */
  PostEntityVo queryDetails(Pageable pageable, Long id, String postKey);

  /**
   * query cover.
   *
   * @param id id
   * @return CoverVo
   */
  CoverVo queryCover(Long id);

  /**
   * query user relationship.
   *
   * @param id     id
   * @param userId userId
   * @return PostUserEntityVo
   */
  PostUserEntityVo queryUserRelationship(Long id, Long userId);

  /**
   * query user relationship.
   *
   * @param id       id
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<PostUserEntityVo> queryUserRelationship(Long id, Pageable pageable);

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