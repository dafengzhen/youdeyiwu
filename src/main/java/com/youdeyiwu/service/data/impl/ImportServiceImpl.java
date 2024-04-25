package com.youdeyiwu.service.data.impl;

import static com.youdeyiwu.tool.Tool.cleanHtmlContent;

import com.youdeyiwu.exception.SectionGroupNotFoundException;
import com.youdeyiwu.exception.SectionNotFoundException;
import com.youdeyiwu.exception.TagNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.model.dto.data.CreatePostImportDto;
import com.youdeyiwu.model.dto.data.CreateSectionGroupImportDto;
import com.youdeyiwu.model.dto.data.CreateSectionImportDto;
import com.youdeyiwu.model.dto.data.CreateTagGroupImportDto;
import com.youdeyiwu.model.dto.data.CreateTagImportDto;
import com.youdeyiwu.model.dto.data.CreateUserImportDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.forum.SectionEntity;
import com.youdeyiwu.model.entity.forum.SectionGroupEntity;
import com.youdeyiwu.model.entity.forum.TagEntity;
import com.youdeyiwu.model.entity.forum.TagGroupEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.forum.SectionGroupRepository;
import com.youdeyiwu.repository.forum.SectionRepository;
import com.youdeyiwu.repository.forum.TagGroupRepository;
import com.youdeyiwu.repository.forum.TagRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.service.data.ImportService;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * import.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class ImportServiceImpl implements ImportService {

  private final UserRepository userRepository;

  private final PasswordEncoder passwordEncoder;

  private final SectionRepository sectionRepository;

  private final SectionGroupRepository sectionGroupRepository;

  private final TagRepository tagRepository;

  private final TagGroupRepository tagGroupRepository;

  private final PostRepository postRepository;

  @Transactional
  @Override
  public List<CreateUserImportDto.UserImportVo> createUsers(CreateUserImportDto dto) {
    Set<CreateUserImportDto.UserImportDto> users = new HashSet<>();
    if (!CollectionUtils.isEmpty(dto.users())) {
      users.addAll(dto.users());
    }

    if (!CollectionUtils.isEmpty(dto.usernames())) {
      dto.usernames()
          .forEach(username -> users.add(new CreateUserImportDto.UserImportDto(username, null)));
    }

    return users
        .stream()
        .filter(user -> StringUtils.hasText(user.username()))
        .map(user -> {
          UserEntity userEntity;
          String username = user.username().trim();
          Optional<UserEntity> byUsername = userRepository.findByUsername(username);
          if (byUsername.isPresent()) {
            userEntity = byUsername.get();
          } else {
            UserEntity entity = new UserEntity();
            entity.setUsername(username);
            userEntity = userRepository.save(entity);
          }

          String password = user.password();
          if (StringUtils.hasText(password)) {
            password = passwordEncoder.encode(password);
          } else if (StringUtils.hasText(dto.unifiedPassword())) {
            password = dto.unifiedPassword();
          }

          if (Objects.nonNull(password)) {
            userEntity.setPassword(passwordEncoder.encode(password));
          }

          return userEntity;
        })
        .map(
            userEntity -> new CreateUserImportDto
                .UserImportVo(userEntity.getId(), userEntity.getUsername())
        )
        .toList();
  }

  @Transactional
  @Override
  public List<CreateSectionImportDto.SectionImportVo> createSections(CreateSectionImportDto dto) {
    if (CollectionUtils.isEmpty(dto.sections())) {
      return List.of();
    }

    return dto.sections()
        .stream()
        .filter(section -> StringUtils.hasText(section.name()))
        .map(section -> {
          SectionEntity sectionEntity;
          String name = section.name().trim();
          Optional<SectionEntity> byName = sectionRepository.findByName(name);
          if (byName.isPresent()) {
            sectionEntity = byName.get();
          } else {
            SectionEntity entity = new SectionEntity();
            entity.setName(name);
            sectionEntity = sectionRepository.save(entity);
          }

          if (StringUtils.hasText(section.overview())) {
            sectionEntity.setOverview(section.overview().trim());
          }

          if (StringUtils.hasText(section.content())) {
            sectionEntity.setContent(cleanHtmlContent(section.content().trim()));
          }

          Set<UserEntity> admins = new HashSet<>();
          if (Objects.nonNull(section.admins())) {
            admins = createUsers(section.admins())
                .stream()
                .map(
                    userVo -> userRepository.findById(userVo.id())
                        .orElseThrow(UserNotFoundException::new)
                )
                .peek(userEntity -> userEntity.getSections().add(sectionEntity))
                .collect(Collectors.toSet());
          }

          Set<TagEntity> tags = new HashSet<>();
          if (Objects.nonNull(section.tags())) {
            tags = createTags(section.tags())
                .stream()
                .map(
                    tagVo -> tagRepository.findById(tagVo.id())
                        .orElseThrow(TagNotFoundException::new)
                )
                .peek(tagEntity -> tagEntity.getSections().add(sectionEntity))
                .collect(Collectors.toSet());
          }

          Set<SectionGroupEntity> sectionGroups = new HashSet<>();
          if (Objects.nonNull(section.sectionGroups())) {
            sectionGroups = createSectionGroups(section.sectionGroups())
                .stream()
                .map(
                    sectionGroupVo -> sectionGroupRepository.findById(sectionGroupVo.id())
                        .orElseThrow(SectionGroupNotFoundException::new)
                )
                .peek(sectionGroupEntity -> sectionGroupEntity.getSections().add(sectionEntity))
                .collect(Collectors.toSet());
          }

          sectionEntity.getAdmins().addAll(admins);
          sectionEntity.getTags().addAll(tags);
          sectionEntity.getSectionGroups().addAll(sectionGroups);
          return sectionEntity;
        })
        .map(
            sectionEntity -> new CreateSectionImportDto
                .SectionImportVo(sectionEntity.getId(), sectionEntity.getName())
        )
        .toList();
  }

  @Transactional
  @Override
  public List<CreateSectionGroupImportDto.SectionGroupImportVo> createSectionGroups(CreateSectionGroupImportDto dto) {
    if (CollectionUtils.isEmpty(dto.sectionGroups())) {
      return List.of();
    }

    return dto.sectionGroups()
        .stream()
        .filter(sectionGroup -> StringUtils.hasText(sectionGroup.name()))
        .map(sectionGroup -> {
          SectionGroupEntity sectionGroupEntity;
          String name = sectionGroup.name().trim();
          Optional<SectionGroupEntity> byName = sectionGroupRepository.findByName(name);
          if (byName.isPresent()) {
            sectionGroupEntity = byName.get();
          } else {
            SectionGroupEntity entity = new SectionGroupEntity();
            entity.setName(name);
            sectionGroupEntity = sectionGroupRepository.save(entity);
          }

          Set<SectionEntity> sections = new HashSet<>();
          if (Objects.nonNull(sectionGroup.sections())) {
            sections = createSections(sectionGroup.sections())
                .stream()
                .map(
                    sectionVo -> sectionRepository.findById(sectionVo.id())
                        .orElseThrow(SectionNotFoundException::new)
                )
                .peek(sectionEntity -> sectionEntity.getSectionGroups().add(sectionGroupEntity))
                .collect(Collectors.toSet());
          }

          sectionGroupEntity.getSections().addAll(sections);
          return sectionGroupEntity;
        })
        .map(
            sectionGroupEntity -> new CreateSectionGroupImportDto
                .SectionGroupImportVo(sectionGroupEntity.getId(), sectionGroupEntity.getName())
        )
        .toList();
  }

  @Transactional
  @Override
  public List<CreateTagImportDto.TagImportVo> createTags(CreateTagImportDto dto) {
    if (CollectionUtils.isEmpty(dto.tags())) {
      return List.of();
    }

    return dto.tags()
        .stream()
        .filter(tag -> StringUtils.hasText(tag.name()))
        .map(tag -> {
          TagEntity tagEntity;
          String name = tag.name().trim();
          Optional<TagEntity> byName = tagRepository.findOptionalByName(name);
          if (byName.isPresent()) {
            tagEntity = byName.get();
          } else {
            TagEntity entity = new TagEntity();
            entity.setName(name);
            tagEntity = tagRepository.save(entity);
          }

          return tagEntity;
        })
        .map(
            tagEntity -> new CreateTagImportDto
                .TagImportVo(tagEntity.getId(), tagEntity.getName())
        )
        .toList();
  }

  @Transactional
  @Override
  public List<CreateTagGroupImportDto.TagGroupImportVo> createTagGroups(CreateTagGroupImportDto dto) {
    if (CollectionUtils.isEmpty(dto.tagGroups())) {
      return List.of();
    }

    return dto.tagGroups()
        .stream()
        .filter(tagGroup -> StringUtils.hasText(tagGroup.name()))
        .map(tagGroup -> {
          TagGroupEntity tagGroupEntity;
          String name = tagGroup.name().trim();
          Optional<TagGroupEntity> byName = tagGroupRepository.findByName(name);
          if (byName.isPresent()) {
            tagGroupEntity = byName.get();
          } else {
            TagGroupEntity entity = new TagGroupEntity();
            entity.setName(name);
            tagGroupEntity = tagGroupRepository.save(entity);
          }

          Set<TagEntity> tags = new HashSet<>();
          if (Objects.nonNull(tagGroup.tags())) {
            tags = createTags(tagGroup.tags())
                .stream()
                .map(
                    tagVo -> tagRepository.findById(tagVo.id())
                        .orElseThrow(TagNotFoundException::new)
                )
                .peek(tagEntity -> tagEntity.getTagGroups().add(tagGroupEntity))
                .collect(Collectors.toSet());
          }

          tagGroupEntity.getTags().addAll(tags);
          return tagGroupEntity;
        })
        .map(
            tagGroupEntity -> new CreateTagGroupImportDto
                .TagGroupImportVo(tagGroupEntity.getId(), tagGroupEntity.getName())
        )
        .toList();
  }

  @Transactional
  @Override
  public List<CreatePostImportDto.PostImportVo> createPosts(CreatePostImportDto dto) {
    if (CollectionUtils.isEmpty(dto.posts())) {
      return List.of();
    }

    return dto.posts()
        .stream()
        .filter(post -> StringUtils.hasText(post.name()))
        .map(post -> {
          PostEntity postEntity;
          String name = post.name().trim();
          Optional<PostEntity> byName = postRepository.findByName(name);
          if (byName.isPresent()) {
            postEntity = byName.get();
          } else {
            PostEntity entity = new PostEntity();
            entity.setName(name);
            postEntity = postRepository.save(entity);
          }

          if (StringUtils.hasText(post.createdOn())) {
            postEntity.setCreatedOn(stringToDateWithShanghaiZone(post.createdOn()));
          }

          if (StringUtils.hasText(post.createdBy())) {
            createUsers(
                new CreateUserImportDto(
                    Set.of(post.createdBy()),
                    null,
                    null
                )
            )
                .stream()
                .map(userVo -> userRepository.findById(userVo.id())
                    .orElseThrow(UserNotFoundException::new))
                .forEach(userEntity -> {
                  postEntity.setCreatedBy(userEntity.getId());
                  postEntity.setUser(userEntity);
                });
          }

          if (StringUtils.hasText(post.overview())) {
            postEntity.setOverview(post.overview().trim());
          }

          if (StringUtils.hasText(post.content())) {
            postEntity.setContent(cleanHtmlContent(post.content().trim()));
          }

          if (StringUtils.hasText(post.contentLink())) {
            postEntity.setContent(post.contentLink().trim());
          }

          if (StringUtils.hasText(post.styles())) {
            postEntity.setContent(post.styles().trim());
          }

          if (StringUtils.hasText(post.classNames())) {
            postEntity.setContent(post.classNames().trim());
          }

          Set<TagEntity> tags = new HashSet<>();
          if (Objects.nonNull(post.tags())) {
            tags = createTags(post.tags())
                .stream()
                .map(
                    tagVo -> tagRepository.findById(tagVo.id())
                        .orElseThrow(TagNotFoundException::new)
                )
                .peek(tagEntity -> tagEntity.getPosts().add(postEntity))
                .collect(Collectors.toSet());
          }

          if (Objects.nonNull(post.sections())) {
            createSections(post.sections())
                .stream()
                .map(
                    sectionVo -> sectionRepository.findById(sectionVo.id())
                        .orElseThrow(SectionNotFoundException::new)
                )
                .forEach(sectionEntity -> {
                  sectionEntity.getPosts().add(postEntity);
                  postEntity.setSection(sectionEntity);
                });
          }

          postEntity.getTags().addAll(tags);
          return postEntity;
        })
        .map(
            postEntity -> new CreatePostImportDto
                .PostImportVo(postEntity.getId(), postEntity.getName())
        )
        .toList();
  }

  /**
   * Converts a date string in the specified format to OffsetDateTime in the Shanghai time zone.
   *
   * @param dateString dateString
   * @return OffsetDateTime
   */
  private OffsetDateTime stringToDateWithShanghaiZone(String dateString) {
    LocalDate localDate = LocalDate.parse(dateString, DateTimeFormatter.ofPattern("yyyy-M-d"));
    return OffsetDateTime.of(localDate, LocalTime.MIDNIGHT, ZoneOffset.ofHours(8));
  }
}