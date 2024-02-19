package com.youdeyiwu.service.forum.impl;

import com.youdeyiwu.enums.forum.PostReviewStateEnum;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.PostReviewQueueNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.PostMapper;
import com.youdeyiwu.mapper.forum.PostReviewQueueMapper;
import com.youdeyiwu.mapper.forum.SectionMapper;
import com.youdeyiwu.mapper.user.UserMapper;
import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.dto.forum.ApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.NotApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.ReceivePostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.RefundPostReviewQueueDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostReviewQueueEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.other.UserContext;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.PostReviewQueueEntityVo;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.PostReviewQueueRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.PostReviewQueueService;
import com.youdeyiwu.tool.I18nTool;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * post review queue.
 *
 * @author dafengzhen
 */
@Log4j2
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class PostReviewQueueServiceImpl implements PostReviewQueueService {

  private final PostReviewQueueRepository postReviewQueueRepository;

  private final PostRepository postRepository;

  private final UserRepository userRepository;

  private final SecurityService securityService;

  private final PostMapper postMapper;

  private final PostReviewQueueMapper postReviewQueueMapper;

  private final UserMapper userMapper;

  private final SectionMapper sectionMapper;

  private final I18nTool i18nTool;

  @Transactional
  @Override
  public void receive(ReceivePostReviewQueueDto dto) {
    PostEntity postEntity = postRepository.findById(dto.postId())
        .orElseThrow(PostNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    if (postEntity.getReviewState() != PostReviewStateEnum.PENDING_REVIEW) {
      throw new CustomException(i18nTool.getMessage("postReviewQueue.pendingReview"));
    }

    boolean whetherToContinue = Boolean.TRUE.equals(userEntity.getRoot())
        || (Objects.nonNull(postEntity.getSection())
        && postEntity.getSection().getAdmins().contains(userEntity));
    if (!whetherToContinue) {
      throw new CustomException(i18nTool.getMessage("postReviewQueue.whetherToContinue"));
    }

    if (postReviewQueueRepository.existsByPost(postEntity)) {
      PostReviewQueueEntity entity = postReviewQueueRepository.findByPost(postEntity);
      if (Boolean.TRUE.equals(entity.getReceived())) {
        throw new CustomException(i18nTool.getMessage("postReviewQueue.received"));
      } else {
        entity.setReceived(true);
        entity.setLatestReviewResultTime(dto.latestReviewResultTime());
        entity.setReceiver(userEntity);
      }
    } else {
      PostReviewQueueEntity entity = new PostReviewQueueEntity();
      entity.setReceived(true);
      entity.setLatestReviewResultTime(dto.latestReviewResultTime());
      entity.setReceiver(userEntity);
      entity.setPost(postEntity);
      postEntity.setPostReviewQueue(entity);
      postReviewQueueRepository.save(entity);
    }
  }

  @Transactional
  @Override
  public void refund(Long id, RefundPostReviewQueueDto dto) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    if (Boolean.FALSE.equals(postReviewQueueEntity.getReceived())) {
      throw new CustomException(i18nTool.getMessage("postReviewQueue.notBeenPicked"));
    }

    if (!Objects.equals(postReviewQueueEntity.getReceiver(), userEntity)) {
      throw new CustomException(i18nTool.getMessage("postReviewQueue.whetherToContinue"));
    }

    postReviewQueueEntity.setReceived(false);
    postReviewQueueEntity.setLatestReviewResultTime(null);
  }

  @Transactional
  @Override
  public void approved(Long id, ApprovedPostReviewQueueDto dto) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    if (Boolean.FALSE.equals(postReviewQueueEntity.getReceived())) {
      throw new CustomException(i18nTool.getMessage("postReviewQueue.notBeenPicked"));
    }

    if (!Objects.equals(postReviewQueueEntity.getReceiver(), userEntity)) {
      throw new CustomException(i18nTool.getMessage("postReviewQueue.whetherToContinue"));
    }

    PostEntity postEntity = postReviewQueueEntity.getPost();
    postEntity.setReviewReason(dto.reason());
    postEntity.setReviewState(PostReviewStateEnum.APPROVED);
    postEntity.setPostReviewQueue(null);
    postReviewQueueRepository.delete(postReviewQueueEntity);
  }

  @Transactional
  @Override
  public void notApproved(Long id, NotApprovedPostReviewQueueDto dto) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    if (Boolean.FALSE.equals(postReviewQueueEntity.getReceived())) {
      throw new CustomException(i18nTool.getMessage("postReviewQueue.notBeenPicked"));
    }

    if (!Objects.equals(postReviewQueueEntity.getReceiver(), userEntity)) {
      throw new CustomException(i18nTool.getMessage("postReviewQueue.whetherToContinue"));
    }

    PostEntity postEntity = postReviewQueueEntity.getPost();
    postEntity.setReviewReason(dto.reason());
    postEntity.setReviewState(PostReviewStateEnum.REJECTED);
    postEntity.setPostReviewQueue(null);
    postReviewQueueRepository.delete(postReviewQueueEntity);
  }

  @Override
  public PostReviewQueueEntityVo query(Long id) {
    PostReviewQueueEntity postReviewQueueEntity = postReviewQueueRepository.findById(id)
        .orElseThrow(PostReviewQueueNotFoundException::new);
    PostReviewQueueEntityVo vo = postReviewQueueMapper.entityToVo(postReviewQueueEntity);
    vo.setPost(postMapper.entityToVo(postReviewQueueEntity.getPost()));
    vo.setReceiver(userMapper.entityToVo(postReviewQueueEntity.getReceiver()));
    return vo;
  }

  @Override
  public PageVo<PostEntityVo> queryAll(Pageable pageable) {
    UserContext userContext = securityService.getUserContext();
    Page<PostEntity> page = postRepository.findPostReviewQueues(
        new PaginationPositionDto(pageable),
        userContext.anonymous(),
        userContext.user(),
        userContext.root()
    );

    return new PageVo<>(
        page.map(postEntity -> {
              PostEntityVo vo = postMapper.entityToVo(postEntity);
              vo.setSection(sectionMapper.entityToVo(postEntity.getSection()));
              PostReviewQueueEntity postReviewQueueEntity = postEntity.getPostReviewQueue();
              PostReviewQueueEntityVo postReviewQueueEntityVo =
                  postReviewQueueMapper.entityToVo(postReviewQueueEntity);

              if (Objects.nonNull(postReviewQueueEntity)) {
                postReviewQueueEntityVo.setReceiver(
                    userMapper.entityToVo(postReviewQueueEntity.getReceiver()));
              }

              vo.setPostReviewQueue(postReviewQueueEntityVo);
              return vo;
            }
        )
    );
  }
}