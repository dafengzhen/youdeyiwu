package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.cleanBasicContent;
import static com.youdeyiwu.tool.Tool.randomUuId;

import com.youdeyiwu.exception.CommentNotFoundException;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.ReplyNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.ReplyMapper;
import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.dto.forum.UpdateStateReplyDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.forum.QuoteReplyEntityVo;
import com.youdeyiwu.repository.forum.CommentRepository;
import com.youdeyiwu.repository.forum.ReplyRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.ReplyService;
import com.youdeyiwu.tool.I18nTool;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * reply.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class ReplyServiceImpl implements ReplyService {

  private final CommentRepository commentRepository;

  private final ReplyRepository replyRepository;

  private final UserRepository userRepository;

  private final ReplyMapper replyMapper;

  private final SecurityService securityService;

  private final I18nTool i18nTool;

  @Transactional
  @Override
  public QuoteReplyEntity create(CreateReplyDto dto) {
    QuoteReplyEntity quoteReplyEntity = new QuoteReplyEntity();
    if (Objects.nonNull(dto.commentId())) {
      CommentEntity commentEntity = commentRepository.findById(dto.commentId())
          .orElseThrow(CommentNotFoundException::new);
      quoteReplyEntity.setComment(commentEntity);
      quoteReplyEntity.setPost(commentEntity.getPost());
    } else if (Objects.nonNull(dto.replyId())) {
      QuoteReplyEntity replyEntity = replyRepository.findById(dto.replyId())
          .orElseThrow(ReplyNotFoundException::new);
      quoteReplyEntity.setComment(replyEntity.getComment());
      quoteReplyEntity.setPost(replyEntity.getPost());
      quoteReplyEntity.setQuoteReply(replyEntity);
    } else {
      throw new CustomException(i18nTool.getMessage("reply.create.invalid"));
    }

    PostEntity postEntity = quoteReplyEntity.getPost();
    postEntity.setRepliesCount(postEntity.getRepliesCount() + 1);
    quoteReplyEntity.setContent(cleanBasicContent(dto.content().trim()));
    quoteReplyEntity.setUniqueIdentifier(randomUuId());

    if (securityService.isAuthenticated()) {
      UserEntity userEntity = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      quoteReplyEntity.setUser(userEntity);
    }

    replyRepository.save(quoteReplyEntity);
    return quoteReplyEntity;
  }

  @Transactional
  @Override
  public void updateState(Long id, UpdateStateReplyDto dto) {
    QuoteReplyEntity quoteReplyEntity = replyRepository.findById(id)
        .orElseThrow(ReplyNotFoundException::new);

    if (Objects.nonNull(dto.reviewState())) {
      quoteReplyEntity.setReviewState(dto.reviewState());
    }
  }

  @Override
  public QuoteReplyEntityVo query(Long id) {
    QuoteReplyEntity quoteReplyEntity = replyRepository.findById(id)
        .orElseThrow(ReplyNotFoundException::new);
    return replyMapper.entityToVo(quoteReplyEntity);
  }
}