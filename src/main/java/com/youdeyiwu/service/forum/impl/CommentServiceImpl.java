package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.cleanBasicContent;
import static com.youdeyiwu.tool.Tool.randomUuId;

import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.CommentMapper;
import com.youdeyiwu.model.dto.forum.CreateCommentDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.repository.forum.CommentRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.CommentService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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

  @Transactional
  @Override
  public CommentEntity create(CreateCommentDto dto) {
    CommentEntity commentEntity = new CommentEntity();
    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);
    postEntity.setCommentsCount(postEntity.getCommentsCount() + 1);
    commentEntity.setPost(postEntity);
    commentEntity.setContent(cleanBasicContent(dto.content().trim()));
    commentEntity.setUniqueIdentifier(randomUuId());

    if (securityService.isAuthenticated()) {
      commentEntity.setUser(
          userRepository.findById(securityService.getUserId())
              .orElseThrow(UserNotFoundException::new)
      );
    }

    commentRepository.save(commentEntity);
    return commentEntity;
  }
}