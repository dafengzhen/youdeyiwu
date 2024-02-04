package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.cleanBasicContent;
import static com.youdeyiwu.tool.Tool.getCurrentDateTime;
import static com.youdeyiwu.tool.Tool.randomUuId;

import com.youdeyiwu.enums.point.RuleNameEnum;
import com.youdeyiwu.enums.point.SignEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.event.PointRuleApplicationEvent;
import com.youdeyiwu.exception.CommentNotFoundException;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.ReplyNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.CommentMapper;
import com.youdeyiwu.mapper.forum.ReplyMapper;
import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.dto.forum.UpdateStateReplyDto;
import com.youdeyiwu.model.dto.point.PointRuleEventDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.forum.QuoteReplyEntityVo;
import com.youdeyiwu.repository.forum.CommentRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.ReplyRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.ReplyService;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
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

  private final UserRepository userRepository;

  private final CommentMapper commentMapper;

  private final ReplyMapper replyMapper;

  private final SecurityService securityService;

  private final ApplicationEventPublisher publisher;

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
    Optional<UserEntity> commentOrReplyUserEntity = Optional.empty();
    QuoteReplyEntity quoteReplyEntity = new QuoteReplyEntity();
    if (Objects.nonNull(dto.commentId())) {
      CommentEntity commentEntity = commentRepository.findById(dto.commentId())
          .orElseThrow(CommentNotFoundException::new);
      quoteReplyEntity.setComment(commentEntity);
      quoteReplyEntity.setPost(commentEntity.getPost());
      postEntity = Optional.of(commentEntity.getPost());
      commentOrReplyUserEntity = Optional.ofNullable(commentEntity.getUser());
    }

    if (Objects.nonNull(dto.replyId())) {
      QuoteReplyEntity replyEntity = replyRepository.findById(dto.replyId())
          .orElseThrow(ReplyNotFoundException::new);
      quoteReplyEntity.setComment(replyEntity.getComment());
      quoteReplyEntity.setPost(replyEntity.getPost());
      quoteReplyEntity.setQuoteReply(replyEntity);
      postEntity = Optional.of(replyEntity.getPost());
      commentOrReplyUserEntity = Optional.ofNullable(replyEntity.getUser());
    }

    postEntity.get().setRepliesCount(postEntity.get().getRepliesCount() + 1);
    quoteReplyEntity.setContent(cleanBasicContent(dto.content().trim()));
    quoteReplyEntity.setUniqueIdentifier(randomUuId());

    if (securityService.isAuthenticated()) {
      final String currentDateTime = getCurrentDateTime();
      UserEntity userEntity = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      quoteReplyEntity.setUser(userEntity);

      if (Objects.nonNull(postEntity.get().getUser())) {
        MessageEntity messageEntity = new MessageEntity();
        messageEntity.setName("You have received a new reply");
        messageEntity.setOverview(
            """
                %s user replied to your article in %s at %s. The reply says: %s.
                """
                .formatted(
                    securityService.getAliasAndId(userEntity),
                    currentDateTime,
                    postEntity.get().getName(),
                    quoteReplyEntity.getContent()
                )
        );
        Map<String, String> content = messageEntity.getContent();
        content.put("postId", postEntity.get().getId().toString());
        content.put("sender", userEntity.getId().toString());
        content.put("receiver", postEntity.get().getUser().getId().toString());
        messageEntity.setLink("/posts/" + postEntity.get().getId());
        messageEntity.setReceiver(postEntity.get().getUser());
        publisher.publishEvent(new MessageApplicationEvent(messageEntity));
      }

      if (
          commentOrReplyUserEntity.isPresent()
              && !Objects.equals(postEntity.get().getUser(), commentOrReplyUserEntity.get())
      ) {
        MessageEntity messageEntity = new MessageEntity();
        messageEntity.setName("You have received a new reply");
        messageEntity.setOverview(
            """
                %s user replied to your comment in the article %s at %s. The reply says: %s
                """
                .formatted(
                    securityService.getAliasAndId(userEntity),
                    currentDateTime,
                    postEntity.get().getName(),
                    quoteReplyEntity.getContent()
                )
        );
        Map<String, String> content = messageEntity.getContent();
        content.put("postId", postEntity.get().getId().toString());

        if (Objects.nonNull(dto.commentId())) {
          content.put("commentId", quoteReplyEntity.getComment().getId().toString());
        }
        if (Objects.nonNull(dto.replyId())) {
          content.put("commentId", quoteReplyEntity.getComment().getId().toString());
          content.put("replyId", quoteReplyEntity.getQuoteReply().getId().toString());
        }

        content.put("sender", userEntity.getId().toString());
        content.put("receiver", commentOrReplyUserEntity.get().getId().toString());
        messageEntity.setLink("/posts/" + postEntity.get().getId());
        messageEntity.setReceiver(commentOrReplyUserEntity.get());
        publisher.publishEvent(new MessageApplicationEvent(messageEntity));
      }

      publisher.publishEvent(new PointRuleApplicationEvent(
          new PointRuleEventDto(
              RuleNameEnum.REPLY_POST,
              SignEnum.POSITIVE,
              false,
              null,
              null,
              postEntity.get().getId(),
              Objects.equals(
                  userEntity,
                  commentOrReplyUserEntity.orElse(null)
              )
                  ? null
                  : commentOrReplyUserEntity.map(user -> Set.of(user.getId())).orElse(null)
          )
      ));
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