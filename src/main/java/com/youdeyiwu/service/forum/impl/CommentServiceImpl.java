package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.cleanBasicContent;
import static com.youdeyiwu.tool.Tool.randomUuId;

import com.youdeyiwu.exception.CommentNotFoundException;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.CommentMapper;
import com.youdeyiwu.model.dto.forum.CreateCommentDto;
import com.youdeyiwu.model.dto.forum.UpdateStateCommentDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.CommentUserEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostHistoryEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.forum.CommentEntityVo;
import com.youdeyiwu.repository.forum.CommentRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.CommentService;
import com.youdeyiwu.tool.I18nTool;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * comment.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class CommentServiceImpl implements CommentService {

  private final CommentRepository commentRepository;

  private final PostRepository postRepository;

  private final UserRepository userRepository;

  private final CommentMapper commentMapper;

  private final SecurityService securityService;

  private final I18nTool i18nTool;

  @Transactional
  @Override
  public CommentEntity create(CreateCommentDto dto) {
    CommentEntity commentEntity = new CommentEntity();
    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);
    checkIfCommentingIsDisabled(postEntity);
    postEntity.setCommentsCount(postEntity.getCommentsCount() + 1);
    commentEntity.setPost(postEntity);
    commentEntity.setContent(cleanBasicContent(dto.content().trim()));
    commentEntity.setUniqueIdentifier(randomUuId());

    if (securityService.isAuthenticated()) {
      UserEntity userEntity = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      commentEntity.setUser(userEntity);
    }

    return commentRepository.save(commentEntity);
  }

  @Transactional
  @Override
  public void updateState(Long id, UpdateStateCommentDto dto) {
    CommentEntity commentEntity = commentRepository.findById(id)
        .orElseThrow(CommentNotFoundException::new);

    if (Objects.nonNull(dto.reviewState())) {
      commentEntity.setReviewState(dto.reviewState());
    }
  }

  @Transactional
  @Override
  public void updateLike(Long id) {
    CommentEntity commentEntity = commentRepository.findById(id)
        .orElseThrow(CommentNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    Optional<CommentUserEntity> commentUserEntityOptional = userEntity.getUserComments()
        .stream()
        .filter(commentUserEntity -> commentUserEntity.getComment().equals(commentEntity)
            && commentUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    CommentUserEntity commentUserEntity;
    if (commentUserEntityOptional.isPresent()) {
      commentUserEntity = commentUserEntityOptional.get();
      commentUserEntity.setLiked(!commentUserEntity.getLiked());
    } else {
      commentUserEntity = new CommentUserEntity();
      commentUserEntity.setLiked(!commentUserEntity.getLiked());
      commentUserEntity.setComment(commentEntity);
      commentUserEntity.setUser(userEntity);
      userEntity.getUserComments().add(commentUserEntity);
      commentEntity.getCommentUsers().add(commentUserEntity);
    }

    commentEntity.setLikesCount(
        Boolean.TRUE.equals(commentUserEntity.getLiked())
            ? commentEntity.getLikesCount() + 1
            : Math.max(0, commentEntity.getLikesCount() - 1)
    );
  }

  @Override
  public CommentEntityVo query(Long id) {
    CommentEntity commentEntity = commentRepository.findById(id)
        .orElseThrow(CommentNotFoundException::new);
    return commentMapper.entityToVo(commentEntity);
  }

  /**
   * check if commenting is disabled.
   *
   * @param postEntity postEntity
   */
  private void checkIfCommentingIsDisabled(PostEntity postEntity) {
    if (Boolean.TRUE.equals(postEntity.getDisableComments())) {
      Optional<PostHistoryEntity> postHistoryEntityOptional = postEntity.getHistories()
          .stream()
          .filter(postHistoryEntity -> Boolean.TRUE.equals(postHistoryEntity.getDisableComments()))
          .findFirst();

      String message = "-";
      if (postHistoryEntityOptional.isPresent()) {
        String commentDisableReason = postHistoryEntityOptional.get().getCommentDisableReason();
        if (StringUtils.hasText(commentDisableReason)) {
          message = commentDisableReason;
        }
      }

      throw new CustomException(i18nTool.getMessage(
          "comment.create.disable",
          Map.of("reason", message)
      ));
    }
  }
}