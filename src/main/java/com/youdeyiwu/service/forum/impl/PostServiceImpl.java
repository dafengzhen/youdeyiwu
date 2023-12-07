package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.cleanHtmlContent;
import static com.youdeyiwu.tool.Tool.isHttpOrHttps;

import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.SectionNotFoundException;
import com.youdeyiwu.exception.TagNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.CommentMapper;
import com.youdeyiwu.mapper.forum.PostMapper;
import com.youdeyiwu.mapper.forum.ReplyMapper;
import com.youdeyiwu.mapper.forum.SectionMapper;
import com.youdeyiwu.mapper.forum.TagGroupMapper;
import com.youdeyiwu.mapper.forum.TagMapper;
import com.youdeyiwu.mapper.user.UserMapper;
import com.youdeyiwu.model.dto.PaginationPositionDto;
import com.youdeyiwu.model.dto.forum.CreatePostDto;
import com.youdeyiwu.model.dto.forum.CreateTagDto;
import com.youdeyiwu.model.dto.forum.QueryParamsPostDto;
import com.youdeyiwu.model.dto.forum.UpdatePostDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsPostDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostUserEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.CommentReplyVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.QuoteReplyEntityVo;
import com.youdeyiwu.repository.forum.CommentRepository;
import com.youdeyiwu.repository.forum.PostFavoriteRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.SectionRepository;
import com.youdeyiwu.repository.forum.TagGroupRepository;
import com.youdeyiwu.repository.forum.TagRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.PostService;
import com.youdeyiwu.service.forum.TagService;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * post.
 *
 * @author dafengzhen
 */
@Log4j2
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class PostServiceImpl implements PostService {

  private final PostRepository postRepository;

  private final PostFavoriteRepository postFavoriteRepository;

  private final CommentRepository commentRepository;

  private final SectionRepository sectionRepository;

  private final UserRepository userRepository;

  private final PostMapper postMapper;

  private final SectionMapper sectionMapper;

  private final TagRepository tagRepository;

  private final TagGroupRepository tagGroupRepository;

  private final TagService tagService;

  private final UserMapper userMapper;

  private final TagMapper tagMapper;

  private final TagGroupMapper tagGroupMapper;

  private final SecurityService securityService;

  private final CommentMapper commentMapper;

  private final ReplyMapper replyMapper;

  @Transactional
  @Override
  public PostEntity create(CreatePostDto dto) {
    PostEntity postEntity = new PostEntity();
    postMapper.dtoToEntity(dto, postEntity);
    setContentAndRelatedLinks(dto.content(), dto.cover(), dto.contentLink(), postEntity);
    setSectionAndTags(dto.sectionId(), dto.tags(), postEntity);
    postRepository.save(postEntity);
    return postEntity;
  }

  @Transactional
  @Override
  public void viewPage(Long id) {
    PostEntity postEntity = findPost(id);
    postEntity.setPageViews(postEntity.getPageViews() + 1);
  }

  @Transactional
  @Override
  public void updateLike(Long id) {
    PostEntity postEntity = findPost(id);
    securityService.checkAuthenticationStatus();
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    Optional<PostUserEntity> postUserEntityOptional = userEntity.getUserPosts()
        .stream()
        .filter(postUserEntity -> postUserEntity.getPost().equals(postEntity)
            && postUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    PostUserEntity postUserEntity;
    if (postUserEntityOptional.isPresent()) {
      postUserEntity = postUserEntityOptional.get();
      postUserEntity.setLiked(!postUserEntity.getLiked());
    } else {
      postUserEntity = new PostUserEntity();
      postUserEntity.setLiked(!postUserEntity.getLiked());
      postUserEntity.setPost(postEntity);
      postUserEntity.setUser(userEntity);
      userEntity.getUserPosts().add(postUserEntity);
      postEntity.getPostUsers().add(postUserEntity);
    }

    postEntity.setLikesCount(
        Boolean.TRUE.equals(postUserEntity.getLiked()) ? postEntity.getLikesCount() + 1
            : Math.max(0, postEntity.getLikesCount() - 1)
    );
  }

  @Transactional
  @Override
  public void updateFavorite(Long id) {
    PostEntity postEntity = findPost(id);
    securityService.checkAuthenticationStatus();
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);
    Optional<PostUserEntity> postUserEntityOptional = userEntity.getUserPosts()
        .stream()
        .filter(postUserEntity -> postUserEntity.getPost().equals(postEntity)
            && postUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    PostUserEntity postUserEntity;
    if (postUserEntityOptional.isPresent()) {
      postUserEntity = postUserEntityOptional.get();
      postUserEntity.setBookmarked(!postUserEntity.getBookmarked());
    } else {
      postUserEntity = new PostUserEntity();
      postUserEntity.setBookmarked(!postUserEntity.getBookmarked());
      postUserEntity.setPost(postEntity);
      postUserEntity.setUser(userEntity);
      userEntity.getUserPosts().add(postUserEntity);
      postEntity.getPostUsers().add(postUserEntity);
    }

    postEntity.setFavoritesCount(
        Boolean.TRUE.equals(postUserEntity.getBookmarked()) ? postEntity.getFavoritesCount() + 1
            : Math.max(0, postEntity.getFavoritesCount() - 1)
    );
  }

  @Transactional
  @Override
  public void updateTags(Long id, UpdateTagsPostDto dto) {
    PostEntity postEntity = findPost(id);

    if (Objects.nonNull(dto.tags())) {
      postEntity.setTags(
          dto.tags()
              .stream()
              .map(tid -> tagRepository.findById(tid).orElseThrow(TagNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void update(Long id, UpdatePostDto dto) {
    PostEntity postEntity = findPost(id);

    postMapper.dtoToEntity(dto, postEntity);
    setContentAndRelatedLinks(dto.content(), dto.cover(), dto.contentLink(), postEntity);
    setSectionAndTags(dto.sectionId(), dto.tags(), postEntity);
  }

  @Override
  public Set<PostEntityVo> queryRandom() {
    return postRepository.findRandomPosts()
        .stream()
        .map(postMapper::entityToVo)
        .collect(Collectors.toSet());
  }

  @Override
  public PageVo<PostEntityVo> selectAll(Pageable pageable, QueryParamsPostDto dto) {
    Page<PostEntity> page;
    if (securityService.isAnonymous()) {
      page = postRepository.findAll(
          new PaginationPositionDto(pageable),
          dto,
          true,
          null
      );
    } else {
      page = postRepository.findAll(
          new PaginationPositionDto(pageable),
          dto,
          false,
          securityService.getUserId()
      );
    }

    return new PageVo<>(
        page.map(postEntity -> {
              PostEntityVo vo = postMapper.entityToVo(postEntity);
              setAdditionalData(vo, postEntity);
              setUser(vo, postEntity);
              return vo;
            }
        )
    );
  }

  @Override
  public PageVo<CommentReplyVo> queryCommentReply(Pageable pageable, Long id) {
    PostEntity postEntity = findPost(id);
    return getCommentReply(pageable, postEntity);
  }

  @Override
  public PostEntityVo queryDetails(Pageable pageable, Long id) {
    PostEntity postEntity = findPost(id);
    PostEntityVo vo = postMapper.entityToVo(postEntity);
    setBadges(vo, postEntity);
    setSection(vo, postEntity);
    setUser(vo, postEntity);
    setTags(vo, postEntity);
    setSocialInteraction(vo, postEntity);
    vo.setComments(getCommentReply(pageable, postEntity));
    return vo;
  }

  @Override
  public PostEntityVo query(Long id) {
    PostEntity postEntity = findPost(id);
    PostEntityVo vo = postMapper.entityToVo(postEntity);
    setAdditionalData(vo, postEntity);
    return vo;
  }

  @Override
  public PageVo<PostEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(postRepository.findAll(pageable).map(postEntity -> {
      PostEntityVo vo = postMapper.entityToVo(postEntity);
      setAdditionalData(vo, postEntity);
      setUser(vo, postEntity);
      return vo;
    }));
  }

  @Transactional
  @Override
  public void delete(Long id) {
    PostEntity postEntity = findPost(id);
    postRepository.delete(postEntity);
  }

  /**
   * find post.
   *
   * @param id id
   * @return PostEntity
   */
  private PostEntity findPost(Long id) {
    return postRepository.findById(id)
        .orElseThrow(PostNotFoundException::new);
  }

  /**
   * set content and related links.
   *
   * @param content     content
   * @param cover       cover
   * @param contentLink contentLink
   * @param postEntity  postEntity
   */
  private void setContentAndRelatedLinks(
      String content,
      String cover,
      String contentLink,
      PostEntity postEntity
  ) {
    if (Objects.nonNull(cover)) {
      if (isHttpOrHttps(cover)) {
        postEntity.setCover(cover);
      } else {
        postEntity.setCover(null);
      }
    }

    if (Objects.nonNull(contentLink)) {
      if (isHttpOrHttps(contentLink)) {
        postEntity.setContentLink(contentLink);
      } else {
        postEntity.setContentLink(null);
      }
    }

    if (Objects.nonNull(content)) {
      postEntity.setContent(cleanHtmlContent(content.trim()));
    }
  }

  /**
   * set section and tags.
   *
   * @param sectionId  sectionId
   * @param tags       tags
   * @param postEntity postEntity
   */
  private void setSectionAndTags(String sectionId, Set<String> tags, PostEntity postEntity) {
    if (Objects.nonNull(sectionId)) {
      if ("none".equals(sectionId)) {
        postEntity.setSection(null);
      } else {
        postEntity.setSection(
            sectionRepository.findById(Long.parseLong(sectionId))
                .orElseThrow(SectionNotFoundException::new)
        );
      }
    }

    if (Objects.nonNull(tags)) {
      postEntity.setTags(
          tags.stream()
              .map(tag -> tagService.create(new CreateTagDto(tag, 0), false))
              .collect(Collectors.toSet())
      );
    }
  }

  /**
   * set additional data.
   *
   * @param vo         vo
   * @param postEntity postEntity
   */
  private void setAdditionalData(PostEntityVo vo, PostEntity postEntity) {
    setBadges(vo, postEntity);
    vo.setImages(
        postEntity.getImages().stream()
            .map(postMapper::entityToVo)
            .collect(Collectors.toSet())
    );
    vo.setAllows(
        postEntity.getAllows().stream()
            .map(userMapper::entityToVo)
            .collect(Collectors.toSet())
    );
    vo.setBlocks(
        postEntity.getBlocks().stream()
            .map(userMapper::entityToVo)
            .collect(Collectors.toSet())
    );
    setTags(vo, postEntity);
    setSection(vo, postEntity);
  }

  /**
   * set user.
   *
   * @param vo         vo
   * @param postEntity postEntity
   */
  private void setUser(PostEntityVo vo, PostEntity postEntity) {
    Long createdBy = postEntity.getCreatedBy();
    if (Objects.isNull(createdBy)) {
      log.debug(
          "=== Post === Empty creator detected for the post, "
              + "perhaps this is an error? "
              + "It could potentially affect the business logic, "
              + "please pay attention to the post ID {}",
          postEntity.getId()
      );
      log.debug(
          "=== Post Name === {}",
          postEntity.getName()
      );
      return;
    }

    vo.setUser(
        userMapper.entityToVo(
            userRepository.findById(postEntity.getCreatedBy())
                .orElseThrow(UserNotFoundException::new)
        )
    );
  }

  /**
   * set section.
   *
   * @param vo         vo
   * @param postEntity postEntity
   */
  private void setSection(PostEntityVo vo, PostEntity postEntity) {
    vo.setSection(sectionMapper.entityToVo(postEntity.getSection()));
  }

  /**
   * set badges.
   *
   * @param vo         vo
   * @param postEntity postEntity
   */
  private void setBadges(PostEntityVo vo, PostEntity postEntity) {
    vo.setBadges(
        postEntity.getBadges().stream()
            .map(postMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set socialInteraction.
   *
   * @param vo         vo
   * @param postEntity postEntity
   */
  private void setSocialInteraction(PostEntityVo vo, PostEntity postEntity) {
    Long createdBy = postEntity.getCreatedBy();
    if (Objects.isNull(createdBy) || securityService.isAnonymous()) {
      return;
    }

    UserEntity userEntity = userRepository.findById(createdBy)
        .orElseThrow(UserNotFoundException::new);
    Optional<PostUserEntity> postUserEntityOptional = userEntity.getUserPosts()
        .stream()
        .filter(postUserEntity -> postUserEntity.getPost().equals(postEntity)
            && postUserEntity.getUser().equals(userEntity)
        )
        .findFirst();

    if (postUserEntityOptional.isPresent()) {
      PostUserEntity postUserEntity = postUserEntityOptional.get();
      vo.setLiked(postUserEntity.getLiked());
      vo.setFollowed(postUserEntity.getFollowed());
      vo.setBookmarked(postUserEntity.getBookmarked());
    }
  }

  /**
   * set tags.
   *
   * @param vo         vo
   * @param postEntity postEntity
   */
  private void setTags(PostEntityVo vo, PostEntity postEntity) {
    vo.setTags(
        postEntity.getTags().stream()
            .map(tagMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * get comment reply.
   *
   * @param pageable   pageable
   * @param postEntity postEntity
   * @return PageVo
   */
  private PageVo<CommentReplyVo> getCommentReply(Pageable pageable, PostEntity postEntity) {
    return new PageVo<>(
        postRepository.findAllCommentReply(
                new PaginationPositionDto(pageable),
                postEntity.getId()
            )
            .map(commentReplyEntityVo -> {
              CommentReplyVo vo = new CommentReplyVo();
              vo.setComment(commentMapper.entityToVo(commentReplyEntityVo.getComment()));
              QuoteReplyEntity quoteReplyEntity = commentReplyEntityVo.getReply();

              if (Objects.nonNull(quoteReplyEntity)) {
                QuoteReplyEntityVo quoteReplyEntityVo = replyMapper.entityToVo(quoteReplyEntity);
                if (Objects.isNull(quoteReplyEntity.getQuoteReply())) {
                  quoteReplyEntityVo.setComment(
                      commentMapper.entityToVo(quoteReplyEntity.getComment())
                  );
                } else {
                  quoteReplyEntityVo.setQuoteReply(
                      replyMapper.entityToVo(quoteReplyEntity.getQuoteReply())
                  );
                }
                vo.setReply(quoteReplyEntityVo);
              }

              return vo;
            })
    );
  }
}