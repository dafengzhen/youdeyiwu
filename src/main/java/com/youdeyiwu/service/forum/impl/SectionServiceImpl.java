package com.youdeyiwu.service.forum.impl;

import static com.youdeyiwu.tool.Tool.cleanHtmlContent;
import static com.youdeyiwu.tool.Tool.getFileType;
import static com.youdeyiwu.tool.Tool.isValidImageFile;

import com.youdeyiwu.enums.file.FileTypeEnum;
import com.youdeyiwu.enums.forum.SectionStateEnum;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.SectionNotFoundException;
import com.youdeyiwu.exception.TagGroupNotFoundException;
import com.youdeyiwu.exception.TagNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.SectionGroupMapper;
import com.youdeyiwu.mapper.forum.SectionMapper;
import com.youdeyiwu.mapper.forum.TagGroupMapper;
import com.youdeyiwu.mapper.forum.TagMapper;
import com.youdeyiwu.mapper.user.UserMapper;
import com.youdeyiwu.model.dto.forum.CreateSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateAdminsSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateCreatePostGuideSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateStatesSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateTagGroupsSectionDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsSectionDto;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.other.UserContext;
import com.youdeyiwu.model.vo.CoverVo;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.SectionEntityVo;
import com.youdeyiwu.repository.forum.SectionRepository;
import com.youdeyiwu.repository.forum.TagGroupRepository;
import com.youdeyiwu.repository.forum.TagRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.forum.SectionService;
import com.youdeyiwu.tool.I18nTool;
import java.io.IOException;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

/**
 * section.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class SectionServiceImpl implements SectionService {

  private final SectionRepository sectionRepository;

  private final UserRepository userRepository;

  private final SectionMapper sectionMapper;

  private final TagRepository tagRepository;

  private final TagGroupRepository tagGroupRepository;

  private final UserMapper userMapper;

  private final TagMapper tagMapper;

  private final TagGroupMapper tagGroupMapper;

  private final SectionGroupMapper sectionGroupMapper;

  private final SecurityService securityService;

  private final I18nTool i18nTool;

  @Transactional
  @Override
  public SectionEntity create(CreateSectionDto dto) {
    SectionEntity sectionEntity = new SectionEntity();
    sectionEntity.setName(dto.name().trim());
    return sectionRepository.save(sectionEntity);
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
          i18nTool.getMessage(
              "section.cover.image.format",
              Map.of("max", 500, "type", "JPG / PNG")
          )
      );
    }

    SectionEntity sectionEntity = findSection(id);
    try {
      sectionEntity.setCoverImage(file.getBytes());
      sectionEntity.setCoverImageType(getFileType(file));
    } catch (IOException e) {
      throw new CustomException(e.getMessage());
    }
  }

  @Transactional
  @Override
  public void updateCreatePostGuide(Long id, UpdateCreatePostGuideSectionDto dto) {
    SectionEntity sectionEntity = findSection(id);

    if (Objects.nonNull(dto.createPostGuide())) {
      sectionEntity.setCreatePostGuide(cleanHtmlContent(dto.createPostGuide().trim()));
    }
  }

  @Transactional
  @Override
  public void updateStates(Long id, UpdateStatesSectionDto dto) {
    SectionEntity sectionEntity = findSection(id);

    if (Objects.nonNull(dto.states())) {
      sectionEntity.setStates(dto.states());
    }

    if (Objects.nonNull(dto.allows())) {
      sectionEntity.setAllows(
          dto.allows()
              .stream()
              .map(uid -> userRepository.findById(uid).orElseThrow(UserNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }

    if (Objects.nonNull(dto.blocks())) {
      sectionEntity.setBlocks(
          dto.blocks()
              .stream()
              .map(uid -> userRepository.findById(uid).orElseThrow(UserNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }

    if (Objects.nonNull(dto.accessKey())) {
      sectionEntity.setAccessKey(dto.accessKey().trim());
    }
  }

  @Transactional
  @Override
  public void updateAdmins(Long id, UpdateAdminsSectionDto dto) {
    SectionEntity sectionEntity = findSection(id);

    if (Objects.nonNull(dto.admins())) {
      sectionEntity.setAdmins(
          dto.admins()
              .stream()
              .map(uid -> userRepository.findById(uid).orElseThrow(UserNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void updateTags(Long id, UpdateTagsSectionDto dto) {
    SectionEntity sectionEntity = findSection(id);

    if (Objects.nonNull(dto.tags())) {
      sectionEntity.setTags(
          dto.tags()
              .stream()
              .map(tid -> tagRepository.findById(tid).orElseThrow(TagNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void updateTagGroups(Long id, UpdateTagGroupsSectionDto dto) {
    SectionEntity sectionEntity = findSection(id);

    if (Objects.nonNull(dto.tagGroups())) {
      sectionEntity.setTagGroups(
          dto.tagGroups()
              .stream()
              .map(tgid -> tagGroupRepository.findById(tgid)
                  .orElseThrow(TagGroupNotFoundException::new)
              )
              .collect(Collectors.toSet())
      );
    }
  }

  @Transactional
  @Override
  public void update(Long id, UpdateSectionDto dto) {
    SectionEntity sectionEntity = findSection(id);
    sectionMapper.dtoToEntity(dto, sectionEntity);

    if (Objects.nonNull(dto.content())) {
      sectionEntity.setContent(cleanHtmlContent(dto.content().trim()));
    }
  }

  @Override
  public SectionEntityVo queryDetails(Long id, String sectionKey) {
    UserContext userContext = securityService.getUserContext();
    SectionEntity sectionEntity = findSection(id);
    checkSectionStates(
        sectionEntity,
        sectionKey,
        userContext.anonymous(),
        userContext.user(),
        userContext.root()
    );
    SectionEntityVo vo = sectionMapper.entityToVo(sectionEntity);
    setAdmins(vo, sectionEntity);
    setSectionGroups(vo, sectionEntity);
    setTagGroup(vo, sectionEntity);
    setTags(vo, sectionEntity);

    if (Objects.nonNull(sectionEntity.getCreatedBy())) {
      vo.setUser(userMapper.entityToVo(
          userRepository.findById(sectionEntity.getCreatedBy())
              .orElseThrow(UserNotFoundException::new)
      ));
    }

    return vo;
  }

  @Override
  public CoverVo queryCover(Long id) {
    SectionEntity sectionEntity = findSection(id);
    byte[] coverImage = sectionEntity.getCoverImage();
    if (Objects.isNull(coverImage)) {
      throw new CustomException(i18nTool.getMessage("section.cover.image.notfound"));
    }

    CoverVo vo = new CoverVo();
    vo.setCoverImage(coverImage);
    vo.setCoverImageType(sectionEntity.getCoverImageType());
    return vo;
  }

  @Override
  public SectionEntityVo query(Long id) {
    SectionEntity sectionEntity = findSection(id);
    SectionEntityVo vo = sectionMapper.entityToVo(sectionEntity);
    setAdmins(vo, sectionEntity);
    vo.setAllows(
        sectionEntity.getAllows()
            .stream()
            .map(userMapper::entityToVo)
            .collect(Collectors.toSet())
    );
    vo.setBlocks(
        sectionEntity.getBlocks()
            .stream()
            .map(userMapper::entityToVo)
            .collect(Collectors.toSet())
    );
    setSectionGroups(vo, sectionEntity);
    setTagGroup(vo, sectionEntity);
    setTags(vo, sectionEntity);
    setCreatePostGuide(vo, sectionEntity);
    return vo;
  }

  @Override
  public List<SectionEntityVo> selectAll(String sectionKey) {
    UserContext userContext = securityService.getUserContext();
    return sectionRepository.findAll(
            sectionKey,
            userContext.anonymous(),
            userContext.user(),
            userContext.root()
        )
        .stream()
        .map(sectionEntity -> {
          SectionEntityVo vo = sectionMapper.entityToVo(sectionEntity);
          setAdmins(vo, sectionEntity);
          setTags(vo, sectionEntity);
          setSectionGroups(vo, sectionEntity);
          setCreatePostGuide(vo, sectionEntity);
          return vo;
        })
        .toList();
  }

  @Override
  public PageVo<SectionEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(sectionRepository.findAll(pageable).map(sectionEntity -> {
      SectionEntityVo vo = sectionMapper.entityToVo(sectionEntity);
      setAdmins(vo, sectionEntity);
      return vo;
    }));
  }

  @Transactional
  @Override
  public void delete(Long id) {
    SectionEntity sectionEntity = findSection(id);
    sectionRepository.delete(sectionEntity);
  }

  /**
   * find section.
   *
   * @param id id
   * @return SectionEntity
   */
  private SectionEntity findSection(Long id) {
    return sectionRepository.findById(id).orElseThrow(SectionNotFoundException::new);
  }

  /**
   * set admins.
   *
   * @param vo            vo
   * @param sectionEntity sectionEntity
   */
  private void setAdmins(SectionEntityVo vo, SectionEntity sectionEntity) {
    vo.setAdmins(
        sectionEntity.getAdmins()
            .stream()
            .map(userMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set tags.
   *
   * @param vo            vo
   * @param sectionEntity sectionEntity
   */
  private void setTags(SectionEntityVo vo, SectionEntity sectionEntity) {
    vo.setTags(
        sectionEntity.getTags()
            .stream()
            .map(tagMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set sectiong group.
   *
   * @param vo            vo
   * @param sectionEntity sectionEntity
   */
  private void setSectionGroups(SectionEntityVo vo, SectionEntity sectionEntity) {
    vo.setSectionGroups(
        sectionEntity.getSectionGroups()
            .stream()
            .map(sectionGroupMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set tag group.
   *
   * @param vo            vo
   * @param sectionEntity sectionEntity
   */
  private void setTagGroup(SectionEntityVo vo, SectionEntity sectionEntity) {
    vo.setTagGroups(
        sectionEntity.getTagGroups()
            .stream()
            .map(tagGroupMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set create post guide.
   *
   * @param vo     vo
   * @param entity entity
   */
  private void setCreatePostGuide(SectionEntityVo vo, SectionEntity entity) {
    vo.setCreatePostGuide(entity.getCreatePostGuide());
  }

  /**
   * check section states.
   *
   * @param sectionEntity sectionEntity
   * @param sectionKey    sectionKey
   * @param anonymous     anonymous
   * @param user          user
   * @param root          root
   */
  private void checkSectionStates(
      SectionEntity sectionEntity,
      String sectionKey,
      boolean anonymous,
      UserEntity user,
      UserEntity root
  ) {
    boolean failed = sectionEntity.getStates().isEmpty()
        || sectionEntity.getStates()
        .stream()
        .map(state -> isAuthorized(state, sectionEntity, sectionKey, anonymous, user, root))
        .anyMatch(Boolean.FALSE::equals);

    if (failed) {
      throw new CustomException(i18nTool.getMessage("section.access"));
    }
  }

  /**
   * is authorized.
   *
   * @param state     state
   * @param section   section
   * @param accessKey accessKey
   * @param anonymous anonymous
   * @param user      user
   * @param root      root
   * @return boolean
   */
  private boolean isAuthorized(
      SectionStateEnum state,
      SectionEntity section,
      String accessKey,
      boolean anonymous,
      UserEntity user,
      UserEntity root
  ) {
    if (
        Objects.nonNull(root)
            || (anonymous && state == SectionStateEnum.SHOW)
    ) {
      return true;
    }

    return switch (state) {
      case SHOW -> true;
      case HIDE -> section.getAdmins().contains(user);
      case LOCK -> Objects.equals(section.getAccessKey(), accessKey)
          || section.getAdmins().contains(user);
      case ALLOW -> Objects.nonNull(user)
          && (section.getAdmins().contains(user)
          || section.getAllows().contains(user));
      case BLOCK -> Objects.nonNull(user)
          && (section.getAdmins().contains(user)
          || section.getBlocks().contains(user));
      case VISIBLE_AFTER_LOGIN -> Objects.nonNull(user);
    };
  }
}