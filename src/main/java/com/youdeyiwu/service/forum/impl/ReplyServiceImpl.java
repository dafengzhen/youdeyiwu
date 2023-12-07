package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.cleanBasicContent;
import static com.youdeyiwu.tool.Tool.randomUuId;

import com.youdeyiwu.exception.CommentNotFoundException;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.ReplyNotFoundException;
import com.youdeyiwu.mapper.forum.CommentMapper;
import com.youdeyiwu.mapper.forum.ReplyMapper;
import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.repository.forum.CommentRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.ReplyRepository;
import com.youdeyiwu.service.forum.ReplyService;
import java.util.Objects;
import java.util.Optional;
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

  private final PostRepository postRepository;

  private final CommentMapper commentMapper;

  private final ReplyMapper replyMapper;

  @Transactional
  @Override
  public QuoteReplyEntity create(CreateReplyDto dto) {
    if (
        (Objects.isNull(dto.commentId()) && Objects.isNull(dto.replyId()))
            || (Objects.nonNull(dto.commentId()) && Objects.nonNull(dto.replyId()))
    ) {
      throw new CustomException("Invalid comment or reply, creation failed");
    }

    Optional<PostEntity> postEntity = Optional.empty();
    QuoteReplyEntity quoteReplyEntity = new QuoteReplyEntity();
    if (Objects.nonNull(dto.commentId())) {
      CommentEntity commentEntity = commentRepository.findById(dto.commentId())
          .orElseThrow(CommentNotFoundException::new);
      quoteReplyEntity.setComment(commentEntity);
      quoteReplyEntity.setPost(commentEntity.getPost());
      postEntity = Optional.of(commentEntity.getPost());
    }

    if (Objects.nonNull(dto.replyId())) {
      QuoteReplyEntity replyEntity = replyRepository.findById(dto.replyId())
          .orElseThrow(ReplyNotFoundException::new);
      quoteReplyEntity.setComment(replyEntity.getComment());
      quoteReplyEntity.setPost(replyEntity.getPost());
      quoteReplyEntity.setQuoteReply(replyEntity);
      postEntity = Optional.of(replyEntity.getPost());
    }

    postEntity.get().setRepliesCount(postEntity.get().getRepliesCount() + 1);
    quoteReplyEntity.setContent(cleanBasicContent(dto.content().trim()));
    quoteReplyEntity.setUniqueIdentifier(randomUuId());
    replyRepository.save(quoteReplyEntity);
    return quoteReplyEntity;
  }
}