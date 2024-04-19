package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.cleanBasicContent;
import static com.youdeyiwu.tool.Tool.randomUuId;

import com.youdeyiwu.constant.RootConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.exception.CommentNotFoundException;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.ReplyNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.ReplyMapper;
import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.dto.forum.UpdateStateReplyDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostHistoryEntity;
import com.youdeyiwu.model.entity.forum.PostUserEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyUserEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.forum.QuoteReplyEntityVo;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.forum.CommentRepository;
import com.youdeyiwu.repository.forum.ReplyRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.ReplyService;
import com.youdeyiwu.tool.I18nTool;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

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

  private final ConfigRepository configRepository;

  @Override
  public void checkDisableAnonymousReplies() {
    Boolean disableAnonymousReplies = configRepository.findOptionalByTypeAndName(
            ConfigTypeEnum.ROOT,
            RootConfigConstant.DISABLE_ANONYMOUS_REPLIES
        )
        .map(configEntity -> Boolean.valueOf(configEntity.getValue()))
        .orElse(false);

    if (Boolean.TRUE.equals(disableAnonymousReplies) && securityService.isAnonymous()) {
      throw new CustomException(i18nTool.getMessage("config.root.disableAnonymousReplies"));
    }
  }

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
    checkIfUserIsReplyBanned();
    checkIfUserPostIsReplyBanned(postEntity);
    checkIfReplyingIsDisabled(postEntity);
    postEntity.setRepliesCount(postEntity.getRepliesCount() + 1);
    quoteReplyEntity.setContent(cleanBasicContent(dto.content().trim()));
    quoteReplyEntity.setUniqueIdentifier(randomUuId());

    if (securityService.isAuthenticated()) {
      UserEntity userEntity = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      quoteReplyEntity.setUser(userEntity);
    }

    return replyRepository.save(quoteReplyEntity);
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

  @Transactional
  @Override
  public void updateLike(Long id) {
    QuoteReplyEntity quoteReplyEntity = replyRepository.findById(id)
        .orElseThrow(ReplyNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    Optional<QuoteReplyUserEntity> quoteReplyUserEntityOptional = userEntity.getUserQuoteReplies()
        .stream()
        .filter(quoteReplyUserEntity -> quoteReplyUserEntity.getQuoteReply().equals(quoteReplyEntity)
            && quoteReplyUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    QuoteReplyUserEntity quoteReplyUserEntity;
    if (quoteReplyUserEntityOptional.isPresent()) {
      quoteReplyUserEntity = quoteReplyUserEntityOptional.get();
      quoteReplyUserEntity.setLiked(!quoteReplyUserEntity.getLiked());
    } else {
      quoteReplyUserEntity = new QuoteReplyUserEntity();
      quoteReplyUserEntity.setLiked(!quoteReplyUserEntity.getLiked());
      quoteReplyUserEntity.setQuoteReply(quoteReplyEntity);
      quoteReplyUserEntity.setUser(userEntity);
      userEntity.getUserQuoteReplies().add(quoteReplyUserEntity);
      quoteReplyEntity.getQuoteReplyUsers().add(quoteReplyUserEntity);
    }

    quoteReplyEntity.setLikesCount(
        Boolean.TRUE.equals(quoteReplyUserEntity.getLiked())
            ? quoteReplyEntity.getLikesCount() + 1
            : Math.max(0, quoteReplyEntity.getLikesCount() - 1)
    );
  }

  @Override
  public QuoteReplyEntityVo query(Long id) {
    QuoteReplyEntity quoteReplyEntity = replyRepository.findById(id)
        .orElseThrow(ReplyNotFoundException::new);
    return replyMapper.entityToVo(quoteReplyEntity);
  }

  /**
   * check if replying is disabled.
   *
   * @param postEntity postEntity
   */
  private void checkIfReplyingIsDisabled(PostEntity postEntity) {
    if (Boolean.TRUE.equals(postEntity.getDisableReplies())) {
      Optional<PostHistoryEntity> postHistoryEntityOptional = postEntity.getHistories()
          .stream()
          .filter(postHistoryEntity -> Boolean.TRUE.equals(postHistoryEntity.getDisableComments()))
          .findFirst();

      String message = "-";
      if (postHistoryEntityOptional.isPresent()) {
        String replyDisableReason = postHistoryEntityOptional.get().getReplyDisableReason();
        if (StringUtils.hasText(replyDisableReason)) {
          message = replyDisableReason;
        }
      }

      throw new CustomException(i18nTool.getMessage(
          "reply.create.disable",
          Map.of("reason", message)
      ));
    }
  }

  /**
   * check if user is reply banned.
   */
  private void checkIfUserIsReplyBanned() {
    if (securityService.isAnonymous()) {
      return;
    }

    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    if (Boolean.TRUE.equals(userEntity.getDisableReplies())) {
      throw new CustomException(
          i18nTool.getMessage(
              "user.reply.create.disable",
              Map.of("reason", userEntity.getReplyDisableReason())
          )
      );
    }
  }

  /**
   * check if user post is reply banned.
   *
   * @param postEntity postEntity
   */
  private void checkIfUserPostIsReplyBanned(PostEntity postEntity) {
    if (securityService.isAnonymous()) {
      return;
    }

    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    Optional<PostUserEntity> postUserEntityOptional = userEntity.getUserPosts()
        .stream()
        .filter(postUserEntity -> postUserEntity.getPost().equals(postEntity)
            && postUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    if (postUserEntityOptional.isEmpty()) {
      return;
    }

    PostUserEntity postUserEntity = postUserEntityOptional.get();
    if (Boolean.TRUE.equals(postUserEntity.getDisableReplies())) {
      throw new CustomException(
          i18nTool.getMessage(
              "user.post.reply.create.disable",
              Map.of("reason", postUserEntity.getReplyDisableReason())
          )
      );
    }
  }
}