package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.cleanHtmlContent;
import static com.youdeyiwu.tool.Tool.getFileType;
import static com.youdeyiwu.tool.Tool.isHttpOrHttps;
import static com.youdeyiwu.tool.Tool.isValidImageFile;

import com.youdeyiwu.enums.file.FileTypeEnum;
import com.youdeyiwu.enums.forum.PostStateEnum;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.PostNotFoundException;
import com.youdeyiwu.exception.SectionGroupNotFoundException;
import com.youdeyiwu.exception.SectionNotFoundException;
import com.youdeyiwu.exception.TagGroupNotFoundException;
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
import com.youdeyiwu.model.dto.forum.QueryParamsPost;
import com.youdeyiwu.model.dto.forum.QueryParamsPostDto;
import com.youdeyiwu.model.dto.forum.UpdatePostDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionPostDto;
import com.youdeyiwu.model.dto.forum.UpdateStatesPostDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsPostDto;
import com.youdeyiwu.model.entity.forum.CommentEntity;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.PostUserEntity;
import com.youdeyiwu.model.entity.forum.QuoteReplyEntity;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.CoverVo;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.CommentEntityVo;
import com.youdeyiwu.model.vo.forum.CommentReplyVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.QuoteReplyEntityVo;
import com.youdeyiwu.repository.forum.CommentRepository;
import com.youdeyiwu.repository.forum.PostFavoriteRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.SectionGroupRepository;
import com.youdeyiwu.repository.forum.SectionRepository;
import com.youdeyiwu.repository.forum.TagGroupRepository;
import com.youdeyiwu.repository.forum.TagRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.PostService;
import com.youdeyiwu.service.forum.TagService;
import java.io.IOException;
import java.util.EnumSet;
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
import org.springframework.web.multipart.MultipartFile;

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

  private final SectionGroupRepository sectionGroupRepository;

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
    setContentAndRelatedLinks(
        dto.content(),
        dto.cover(),
        dto.coverImage(),
        dto.contentLink(),
        postEntity
    );
    setSectionAndTags(dto.sectionId(), false, dto.tags(), postEntity);

    if (securityService.isAuthenticated()) {
      postEntity.setUser(
          userRepository.findById(securityService.getUserId())
              .orElseThrow(UserNotFoundException::new)
      );
    }

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
  public void uploadCover(Long id, MultipartFile file) {
    if (!isValidImageFile(
        file,
        500,
        EnumSet.of(FileTypeEnum.JPG, FileTypeEnum.PNG)
    )) {
      throw new CustomException(
          "This doesn't seem to be a valid cover image file"
      );
    }

    PostEntity postEntity = findPost(id);
    try {
      postEntity.setCoverImage(file.getBytes());
      postEntity.setCoverImageType(getFileType(file));
    } catch (IOException e) {
      throw new CustomException(
          "The setting of the cover image file failed : " + e.getMessage()
      );
    }
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
  public void updateSection(Long id, UpdateSectionPostDto dto) {
    PostEntity postEntity = findPost(id);

    if (Objects.nonNull(dto.sectionId())) {
      postEntity.setSection(
          sectionRepository.findById(dto.sectionId())
              .orElseThrow(SectionNotFoundException::new)
      );
    }

    if (Boolean.TRUE.equals(dto.removeSection())) {
      postEntity.setSection(null);
    }
  }

  @Transactional
  @Override
  public void updateStates(Long id, UpdateStatesPostDto dto) {
    PostEntity postEntity = findPost(id);

    if (Objects.nonNull(dto.reviewState())) {
      postEntity.setReviewState(dto.reviewState());
    }

    if (Objects.nonNull(dto.sortState())) {
      postEntity.setSortState(dto.sortState());
    }

    if (Objects.nonNull(dto.states())) {
      postEntity.setStates(dto.states());
    }

    if (Objects.nonNull(dto.allows())) {
      postEntity.setAllows(
          dto.allows()
              .stream()
              .map(uid -> userRepository.findById(uid).orElseThrow(UserNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }

    if (Objects.nonNull(dto.blocks())) {
      postEntity.setBlocks(
          dto.blocks()
              .stream()
              .map(uid -> userRepository.findById(uid).orElseThrow(UserNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }

    if (Objects.nonNull(dto.accessKey())) {
      postEntity.setAccessKey(dto.accessKey());
    }
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
    setContentAndRelatedLinks(
        dto.content(),
        dto.cover(),
        dto.coverImage(),
        dto.contentLink(),
        postEntity
    );

    setSectionAndTags(dto.sectionId(), dto.removeSection(), dto.tags(), postEntity);
  }

  @Override
  public Set<PostEntityVo> queryRandom() {
    return postRepository.findRandomPosts()
        .stream()
        .map(postMapper::entityToVo)
        .collect(Collectors.toSet());
  }

  @Override
  public PageVo<PostEntityVo> selectAll(
      Pageable pageable,
      QueryParamsPostDto dto,
      String postKey
  ) {
    UserEntity user = null;
    UserEntity root = null;
    boolean anonymous = securityService.isAnonymous();

    if (!anonymous) {
      user = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      if (Boolean.TRUE.equals(user.getRoot())) {
        root = user;
      }
    }

    Page<PostEntity> page = postRepository.findAll(
        new PaginationPositionDto(pageable),
        new QueryParamsPost(
            Objects.nonNull(dto.sectionGroupId())
                ? sectionGroupRepository.findById(dto.sectionGroupId())
                .orElseThrow(SectionGroupNotFoundException::new)
                : null,
            Objects.nonNull(dto.sectionId())
                ? sectionRepository.findById(dto.sectionId())
                .orElseThrow(SectionNotFoundException::new)
                : null,
            Objects.nonNull(dto.tagGroupId())
                ? tagGroupRepository.findById(dto.tagGroupId())
                .orElseThrow(TagGroupNotFoundException::new)
                : null,
            Objects.nonNull(dto.tagId())
                ? tagRepository.findById(dto.tagId())
                .orElseThrow(TagNotFoundException::new)
                : null
        ),
        postKey,
        anonymous,
        user,
        root
    );

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
    UserEntity user = null;
    UserEntity root = null;
    boolean anonymous = securityService.isAnonymous();

    if (!anonymous) {
      user = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      if (Boolean.TRUE.equals(user.getRoot())) {
        root = user;
      }
    }

    PostEntity postEntity = findPost(id);
    return getCommentReply(pageable, postEntity, anonymous, user, root);
  }

  @Override
  public PostEntityVo queryDetails(Pageable pageable, Long id, String postKey) {
    UserEntity user = null;
    UserEntity root = null;
    boolean anonymous = securityService.isAnonymous();

    if (!anonymous) {
      user = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      if (Boolean.TRUE.equals(user.getRoot())) {
        root = user;
      }
    }

    PostEntity postEntity = findPost(id);
    checkPostStates(postEntity, postKey, anonymous, user, root);
    PostEntityVo vo = postMapper.entityToVo(postEntity);
    setBadges(vo, postEntity);
    setSection(vo, postEntity);
    setUser(vo, postEntity);
    setTags(vo, postEntity);
    setSocialInteraction(vo, postEntity);
    vo.setComments(getCommentReply(pageable, postEntity, anonymous, user, root));
    return vo;
  }

  @Override
  public CoverVo queryCover(Long id) {
    PostEntity postEntity = findPost(id);
    byte[] coverImage = postEntity.getCoverImage();
    if (Objects.isNull(coverImage)) {
      throw new CustomException("The cover image file does not exist");
    }

    CoverVo vo = new CoverVo();
    vo.setCoverImage(coverImage);
    vo.setCoverImageType(postEntity.getCoverImageType());
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
   * @param coverImage  coverImage
   * @param contentLink contentLink
   * @param postEntity  postEntity
   */
  private void setContentAndRelatedLinks(
      String content,
      String cover,
      MultipartFile coverImage,
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

    if (
        Objects.nonNull(coverImage)
            && isValidImageFile(
            coverImage,
            500,
            EnumSet.of(FileTypeEnum.JPG, FileTypeEnum.PNG)
        )
    ) {
      try {
        postEntity.setCoverImage(coverImage.getBytes());
        postEntity.setCoverImageType(getFileType(coverImage));
      } catch (IOException e) {
        throw new CustomException(
            "The setting of the cover image file failed : " + e.getMessage()
        );
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
   * @param sectionId     sectionId
   * @param removeSection removeSection
   * @param tags          tags
   * @param postEntity    postEntity
   */
  private void setSectionAndTags(
      Long sectionId,
      Boolean removeSection,
      Set<String> tags,
      PostEntity postEntity
  ) {
    if (Objects.nonNull(sectionId)) {
      postEntity.setSection(
          sectionRepository.findById(sectionId)
              .orElseThrow(SectionNotFoundException::new)
      );
    }

    if (Boolean.TRUE.equals(removeSection)) {
      postEntity.setSection(null);
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
   * @param pageable    pageable
   * @param postEntity  postEntity
   * @param isAnonymous isAnonymous
   * @param user        user
   * @param root        root
   * @return PageVo
   */
  private PageVo<CommentReplyVo> getCommentReply(
      Pageable pageable,
      PostEntity postEntity,
      Boolean isAnonymous,
      UserEntity user,
      UserEntity root
  ) {
    return new PageVo<>(
        postRepository.findAllCommentReply(
                new PaginationPositionDto(pageable),
                postEntity,
                isAnonymous,
                user,
                root
            )
            .map(commentReplyEntityVo -> {
              CommentReplyVo vo = new CommentReplyVo();
              setComment(vo, commentReplyEntityVo.getComment());
              setReply(vo, commentReplyEntityVo.getReply());
              return vo;
            })
    );
  }

  /**
   * set comment.
   *
   * @param vo            vo
   * @param commentEntity commentEntity
   */
  private void setComment(CommentReplyVo vo, CommentEntity commentEntity) {
    if (Objects.isNull(commentEntity)) {
      return;
    }

    CommentEntityVo commentEntityVo = commentMapper.entityToVo(commentEntity);
    commentEntityVo.setUser(userMapper.entityToVo(commentEntity.getUser()));
    vo.setComment(commentEntityVo);
  }

  /**
   * set reply.
   *
   * @param vo               vo
   * @param quoteReplyEntity quoteReplyEntity
   */
  private void setReply(CommentReplyVo vo, QuoteReplyEntity quoteReplyEntity) {
    if (Objects.isNull(quoteReplyEntity)) {
      return;
    }

    QuoteReplyEntityVo quoteReplyEntityVo = replyMapper.entityToVo(quoteReplyEntity);
    QuoteReplyEntity quoteReply = quoteReplyEntity.getQuoteReply();

    if (Objects.isNull(quoteReply)) {
      CommentEntity comment = quoteReplyEntity.getComment();
      CommentEntityVo entityVo = commentMapper.entityToVo(comment);
      entityVo.setUser(userMapper.entityToVo(comment.getUser()));
      quoteReplyEntityVo.setComment(entityVo);
    } else {
      QuoteReplyEntityVo entityVo =
          replyMapper.entityToVo(quoteReply);
      entityVo.setUser(userMapper.entityToVo(quoteReply.getUser()));
      quoteReplyEntityVo.setQuoteReply(entityVo);
    }

    quoteReplyEntityVo.setUser(userMapper.entityToVo(quoteReplyEntity.getUser()));
    vo.setReply(quoteReplyEntityVo);
  }

  /**
   * check post states.
   *
   * @param postEntity postEntity
   * @param postKey    postKey
   * @param anonymous  anonymous
   * @param user       user
   * @param root       root
   */
  private void checkPostStates(
      PostEntity postEntity,
      String postKey,
      boolean anonymous,
      UserEntity user,
      UserEntity root
  ) {
    boolean failed = postEntity.getStates().isEmpty()
        || postEntity.getStates()
        .stream()
        .map(state -> isAuthorized(state, postEntity, postKey, anonymous, user, root))
        .anyMatch(Boolean.FALSE::equals);

    if (failed) {
      throw new CustomException(
          "Sorry, cannot access this post, or this post does not exist");
    }
  }

  /**
   * is authorized.
   *
   * @param state     state
   * @param post      post
   * @param accessKey accessKey
   * @param anonymous anonymous
   * @param user      user
   * @param root      root
   * @return boolean
   */
  private boolean isAuthorized(
      PostStateEnum state,
      PostEntity post,
      String accessKey,
      boolean anonymous,
      UserEntity user,
      UserEntity root
  ) {
    if (Objects.nonNull(root)) {
      return true;
    }

    if (anonymous && state != PostStateEnum.SHOW) {
      return false;
    }

    SectionEntity section = post.getSection();
    return switch (state) {
      case SHOW -> true;
      case HIDE -> Objects.nonNull(section) && section.getAdmins().contains(user);
      case LOCK -> Objects.equals(post.getAccessKey(), accessKey)
          || (Objects.nonNull(section) && section.getAdmins().contains(user));
      case ALLOW -> Objects.nonNull(user)
          && ((Objects.nonNull(section) && section.getAdmins().contains(user))
          || post.getAllows().contains(user));
      case BLOCK -> Objects.nonNull(user)
          && ((Objects.nonNull(section) && section.getAdmins().contains(user))
          || post.getBlocks().contains(user));
      case VISIBLE_AFTER_LOGIN -> Objects.nonNull(user);
    };
  }
}