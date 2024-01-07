package com.youdeyiwu.service.user.impl;

import static com.youdeyiwu.tool.JwtTool.createJwt;
import static com.youdeyiwu.tool.JwtTool.decodeSecret;
import static com.youdeyiwu.tool.Tool.getCurrentDateTime;

import com.youdeyiwu.constant.JwtConfigConstant;
import com.youdeyiwu.enums.config.ConfigTypeEnum;
import com.youdeyiwu.event.MessageApplicationEvent;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.RoleNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.forum.PostMapper;
import com.youdeyiwu.mapper.forum.SectionMapper;
import com.youdeyiwu.mapper.forum.TagMapper;
import com.youdeyiwu.mapper.user.ActionMapper;
import com.youdeyiwu.mapper.user.MenuMapper;
import com.youdeyiwu.mapper.user.PermissionMapper;
import com.youdeyiwu.mapper.user.RoleMapper;
import com.youdeyiwu.mapper.user.SubmenuMapper;
import com.youdeyiwu.mapper.user.UserMapper;
import com.youdeyiwu.model.dto.user.AssignRolesDto;
import com.youdeyiwu.model.dto.user.LoginDto;
import com.youdeyiwu.model.dto.user.RegisterDto;
import com.youdeyiwu.model.dto.user.UpdateRolesUserDto;
import com.youdeyiwu.model.dto.user.UpdateUserPasswordDto;
import com.youdeyiwu.model.dto.user.UpdateUserProfileDto;
import com.youdeyiwu.model.dto.user.UpdateUserStatesDto;
import com.youdeyiwu.model.dto.user.UpdateUserUsernameDto;
import com.youdeyiwu.model.dto.user.UsersCountByDateDto;
import com.youdeyiwu.model.entity.forum.PostEntity;
import com.youdeyiwu.model.entity.message.MessageEntity;
import com.youdeyiwu.model.entity.user.ActionEntity;
import com.youdeyiwu.model.entity.user.RoleEntity;
import com.youdeyiwu.model.entity.user.SubmenuEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.TokenVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.PostFavoriteEntityVo;
import com.youdeyiwu.model.vo.user.MenuEntityVo;
import com.youdeyiwu.model.vo.user.PermissionEntityVo;
import com.youdeyiwu.model.vo.user.RoleEntityVo;
import com.youdeyiwu.model.vo.user.SubmenuEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import com.youdeyiwu.model.vo.user.UserRolesPermissionsVo;
import com.youdeyiwu.model.vo.user.UserStatisticsVo;
import com.youdeyiwu.model.vo.user.UsersCountByDateVo;
import com.youdeyiwu.repository.config.ConfigRepository;
import com.youdeyiwu.repository.forum.PostFavoriteRepository;
import com.youdeyiwu.repository.forum.PostRepository;
import com.youdeyiwu.repository.user.ActionRepository;
import com.youdeyiwu.repository.user.MenuRepository;
import com.youdeyiwu.repository.user.RoleRepository;
import com.youdeyiwu.repository.user.SubmenuRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.user.UserService;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.userdetails.UserCache;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

/**
 * user.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class UserServiceImpl implements UserService {

  private final PasswordEncoder passwordEncoder;

  private final UserRepository userRepository;

  private final PostRepository postRepository;

  private final PostFavoriteRepository postFavoriteRepository;

  private final ConfigRepository configRepository;

  private final RoleRepository roleRepository;

  private final UserMapper userMapper;

  private final RoleMapper roleMapper;

  private final PermissionMapper permissionMapper;

  private final SecurityService securityService;

  private final PostMapper postMapper;

  private final SectionMapper sectionMapper;

  private final TagMapper tagMapper;

  private final UserCache userCache;

  private final MenuMapper menuMapper;

  private final SubmenuMapper submenuMapper;

  private final ActionMapper actionMapper;

  private final MenuRepository menuRepository;

  private final SubmenuRepository submenuRepository;

  private final ActionRepository actionRepository;

  private final ApplicationEventPublisher publisher;

  @Transactional
  @Override
  public TokenVo register(RegisterDto dto) {
    final String username = dto.username().trim();
    final String password = dto.password().trim();
    final String alias = StringUtils.hasText(dto.alias()) ? dto.alias().trim() : username;

    if (Boolean.TRUE.equals(userRepository.existsByUsername(username))) {
      throw new CustomException("This username already exists");
    }

    UserEntity entity = new UserEntity();
    entity.setAlias(alias);
    entity.setUsername(username);
    entity.setPassword(passwordEncoder.encode(password));
    UserEntity userEntity = userRepository.save(entity);

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("Welcome notification");
    messageEntity.setOverview(
        """
            Welcome, %s!
            Thank you for registering with us.
            Your registration was successful at %s.
            We are excited to have you on board!
            """
            .formatted(alias, getCurrentDateTime())
    );
    messageEntity.setReceiver(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
    return createToken(userEntity);
  }

  @Transactional
  @Override
  public TokenVo login(LoginDto dto) {
    final String username = dto.username().trim();
    final String password = dto.password().trim();

    if (Boolean.FALSE.equals(userRepository.existsByUsername(username))) {
      throw new UserNotFoundException();
    }

    UserEntity userEntity = userRepository.findByUsername(username);
    if (!passwordEncoder.matches(password, userEntity.getPassword())) {
      throw new CustomException("Wrong username or password");
    }

    MessageEntity messageEntity = new MessageEntity();
    messageEntity.setName("Login notification");
    messageEntity.setOverview(
        """
            Congratulations on your successful login.
            You performed an operation at %s time
            """
            .formatted(getCurrentDateTime())
    );
    messageEntity.setReceiver(userEntity);
    publisher.publishEvent(new MessageApplicationEvent(messageEntity));
    return createToken(userEntity);
  }

  @Transactional
  @Override
  public void logout() {
    if (securityService.isAnonymous()) {
      throw new CustomException("The inability to log out");
    }

    UserEntity userEntity = findUser(securityService.getUserId());
    userEntity.setToken(null);
    userCache.removeUserFromCache(String.valueOf(userEntity.getId()));
  }

  @Transactional
  @Override
  public void addRoles(Long id, AssignRolesDto dto) {
    UserEntity userEntity = findUser(id);
    List<RoleEntity> roles = dto.ids().stream()
        .map(rid -> roleRepository.findById(rid)
            .orElseThrow(RoleNotFoundException::new)
        )
        .toList();

    userEntity.getRoles().addAll(roles);
  }

  @Transactional
  @Override
  public void removeRoles(Long id, AssignRolesDto dto) {
    UserEntity userEntity = findUser(id);

    List<RoleEntity> roles = dto.ids().stream()
        .map(rid -> roleRepository.findById(rid)
            .orElseThrow(RoleNotFoundException::new)
        )
        .toList();

    roles.forEach(userEntity.getRoles()::remove);
  }

  @Transactional
  @Override
  public void updateRoles(Long id, UpdateRolesUserDto dto) {
    UserEntity userEntity = findUser(id);

    if (Objects.nonNull(dto.roles())) {
      userEntity.setRoles(
          dto.roles()
              .stream()
              .map(rid -> roleRepository.findById(rid).orElseThrow(RoleNotFoundException::new))
              .collect(Collectors.toSet())
      );
    }
  }

  @Override
  public UserRolesPermissionsVo getRolesPermissions(Long id) {
    UserEntity userEntity = findUser(id);

    List<RoleEntityVo> roles = new ArrayList<>();
    Set<PermissionEntityVo> permissions = new HashSet<>();
    userEntity.getRoles().forEach(roleEntity -> {
      RoleEntityVo roleEntityVo = roleMapper.entityToVo(roleEntity);
      roleEntity.getPermissions().forEach(permissionEntity -> {
        PermissionEntityVo permissionEntityVo = permissionMapper.entityToVo(permissionEntity);
        permissionEntityVo.setRole(roleEntityVo);
        permissions.add(permissionEntityVo);
      });
      roles.add(roleEntityVo);
    });

    UserRolesPermissionsVo vo = new UserRolesPermissionsVo();
    vo.setUser(userMapper.entityToVo(userEntity));
    vo.setRoles(roles);
    vo.setPermissions(permissions);
    return vo;
  }

  @Transactional
  @Override
  public void updateProfile(Long id, UpdateUserProfileDto dto) {
    UserEntity userEntity = findUser(id);

    if (StringUtils.hasText(dto.alias())) {
      userEntity.setAlias(dto.alias().trim());
    }

    if (
        StringUtils.hasText(dto.avatar())
            && (dto.avatar().startsWith("http") || dto.avatar().startsWith("https"))
    ) {
      userEntity.setAvatar(dto.avatar().trim());
    }

    if (StringUtils.hasText(dto.oneSentence())) {
      userEntity.setOneSentence(dto.oneSentence().trim());
    }
  }

  @Transactional
  @Override
  public void updateUsername(Long id, UpdateUserUsernameDto dto) {
    UserEntity userEntity = findUser(id);

    if (Boolean.TRUE.equals(userRepository.existsByUsername(dto.username()))) {
      throw new CustomException("This username already exists");
    }

    userEntity.setUsername(dto.username());
  }

  @Transactional
  @Override
  public void updatePassword(Long id, UpdateUserPasswordDto dto) {
    UserEntity userEntity = findUser(id);

    if (!passwordEncoder.matches(dto.oldPassword(), userEntity.getPassword())) {
      throw new CustomException("Does not match old password, update failed");
    }

    userEntity.setPassword(passwordEncoder.encode(dto.newPassword()));
  }

  @Transactional
  @Override
  public void updateStates(Long id, UpdateUserStatesDto dto) {
    UserEntity userEntity = findUser(id);

    if (Objects.nonNull(dto.accountNonExpired())) {
      userEntity.setAccountNonExpired(Boolean.TRUE.equals(dto.accountNonExpired()));
    }

    if (Objects.nonNull(dto.accountNonLocked())) {
      userEntity.setAccountNonLocked(Boolean.TRUE.equals(dto.accountNonLocked()));
    }

    if (Objects.nonNull(dto.credentialsNonExpired())) {
      userEntity.setCredentialsNonExpired(Boolean.TRUE.equals(dto.credentialsNonExpired()));
    }

    if (Objects.nonNull(dto.enabled())) {
      userEntity.setEnabled(Boolean.TRUE.equals(dto.enabled()));
    }
  }

  @Override
  public Set<MenuEntityVo> getMenus() {
    if (securityService.isAnonymous()) {
      return getAnonymousUserMenus();
    }
    return getAuthenticatedUserMenus();
  }

  @Override
  public UserEntityVo getLoginInfo() {
    if (securityService.isAnonymous()) {
      return null;
    }

    UserEntity userEntity = findUser(securityService.getUserId());
    UserEntityVo vo = userMapper.entityToVo(userEntity);
    setRoles(vo, userEntity);
    return vo;
  }

  @Override
  public List<UsersCountByDateVo> getUsersCountByDate(UsersCountByDateDto dto) {
    LocalDateTime today = LocalDateTime.now();
    LocalDateTime pastDate = today.minusDays(Objects.isNull(dto.pastDays()) ? 15 : dto.pastDays());
    return userRepository.getUsersCountByDate(pastDate, today);
  }

  @Override
  public PageVo<UserEntityVo> selectAll(Pageable pageable) {
    return new PageVo<>(userRepository.findAll(pageable).map(userEntity -> {
      UserEntityVo vo = userMapper.entityToVo(userEntity);
      setRoles(vo, userEntity);
      return vo;
    }));
  }

  @Override
  public UserEntityVo queryDetails(Long id) {
    UserEntity userEntity = findUser(id);
    UserEntityVo vo = userMapper.entityToVo(userEntity);
    setRoles(vo, userEntity);

    if (securityService.isAuthenticated()) {
      setFavorites(vo, userEntity);
    }

    Set<PostEntity> posts = postRepository.findAllByUser(userEntity);
    setPosts(vo, posts);
    vo.setRelatedSections(
        posts
            .stream()
            .map(PostEntity::getSection)
            .filter(Objects::nonNull)
            .map(sectionMapper::entityToVo)
            .collect(Collectors.toSet())
    );
    vo.setRelatedTags(
        posts
            .stream()
            .flatMap(postEntity -> postEntity.getTags().stream())
            .map(tagMapper::entityToVo)
            .collect(Collectors.toSet())
    );
    setRelatedStatistics(vo);
    return vo;
  }

  @Override
  public UserEntityVo query(Long id) {
    UserEntity userEntity = findUser(id);
    UserEntityVo vo = userMapper.entityToVo(userEntity);
    setPosts(vo, userEntity);
    setRoles(vo, userEntity);
    return vo;
  }

  @Override
  public PageVo<UserEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(userRepository.findAll(pageable).map(userEntity -> {
      UserEntityVo vo = userMapper.entityToVo(userEntity);
      setRoles(vo, userEntity);
      return vo;
    }));
  }

  @Transactional
  @Override
  public void delete(Long id) {
    UserEntity userEntity = findUser(id);
    userRepository.delete(userEntity);
  }

  /**
   * find user.
   *
   * @param id id
   * @return UserEntity
   */
  private UserEntity findUser(Long id) {
    return userRepository.findById(id)
        .orElseThrow(UserNotFoundException::new);
  }

  /**
   * create token.
   *
   * @param userEntity userEntity
   * @return TokenVo
   */
  private TokenVo createToken(UserEntity userEntity) {
    int expDays = 31;
    String secret = configRepository
        .findByTypeAndName(ConfigTypeEnum.JWT, JwtConfigConstant.SECRET)
        .getValue();

    String token = createJwt(decodeSecret(secret), userEntity.getId(), Duration.ofDays(expDays));
    userEntity.setToken(token);
    userEntity.setLastLoginTime(LocalDateTime.now());
    TokenVo tokenVo = new TokenVo();
    tokenVo.setId(userEntity.getId());
    tokenVo.setAlias(userEntity.getAlias());
    tokenVo.setToken(token);
    tokenVo.setExpDays(expDays);
    userCache.removeUserFromCache(String.valueOf(userEntity.getId()));
    return tokenVo;
  }

  /**
   * set posts.
   *
   * @param vo         vo
   * @param userEntity userEntity
   */
  private void setPosts(UserEntityVo vo, UserEntity userEntity) {
    setPosts(vo, postRepository.findAllByUser(userEntity));
  }

  /**
   * set posts.
   *
   * @param vo    vo
   * @param posts posts
   */
  private void setPosts(UserEntityVo vo, Set<PostEntity> posts) {
    vo.setPosts(
        posts
            .stream()
            .map(postMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set roles.
   *
   * @param vo         vo
   * @param userEntity userEntity
   */
  private void setRoles(UserEntityVo vo, UserEntity userEntity) {
    vo.setRoles(
        userEntity.getRoles()
            .stream()
            .map(roleMapper::entityToVo)
            .collect(Collectors.toSet())
    );
  }

  /**
   * set favorites.
   *
   * @param vo         vo
   * @param userEntity userEntity
   */
  private void setFavorites(UserEntityVo vo, UserEntity userEntity) {
    vo.setFavorites(
        postFavoriteRepository.findAllByUser(userEntity)
            .stream()
            .map(postFavoriteEntity -> {
              PostFavoriteEntityVo postFavoriteEntityVo = postMapper.entityToVo(postFavoriteEntity);
              postFavoriteEntityVo.setPostId(postFavoriteEntity.getPost().getId());
              return postFavoriteEntityVo;
            })
            .collect(Collectors.toSet())
    );
  }

  /**
   * set related statistics.
   *
   * @param vo vo
   */
  private void setRelatedStatistics(UserEntityVo vo) {
    UserStatisticsVo userStatisticsVo = new UserStatisticsVo();
    userStatisticsVo.setSections(vo.getRelatedSections().size());
    userStatisticsVo.setTags(vo.getRelatedTags().size());
    userStatisticsVo.setPosts(vo.getPosts().size());
    userStatisticsVo.setViews(
        (int) vo.getPosts()
            .stream()
            .mapToLong(PostEntityVo::getPageViews)
            .count()
    );
    userStatisticsVo.setComments(
        (int) vo.getPosts()
            .stream()
            .mapToLong(PostEntityVo::getCommentsCount)
            .count()
    );
    userStatisticsVo.setReplies(
        (int) vo.getPosts()
            .stream()
            .mapToLong(PostEntityVo::getRepliesCount)
            .count()
    );
    vo.setRelatedStatistics(userStatisticsVo);
  }

  /**
   * get anonymous user menus.
   *
   * @return Set
   */
  private Set<MenuEntityVo> getAnonymousUserMenus() {
    return StreamSupport.stream(
            menuRepository.findAll(Sort.by(Sort.Direction.DESC, "sort", "id")).spliterator(),
            false
        )
        .filter(menuEntity -> menuEntity.getRoles().isEmpty())
        .map(menuEntity -> {
          MenuEntityVo vo = menuMapper.entityToVo(menuEntity);
          if (menuEntity.getSubmenus().isEmpty()) {
            vo.setActions(
                menuEntity.getActions()
                    .stream()
                    .filter(actionEntity -> Objects.isNull(actionEntity.getRole()))
                    .sorted(
                        Comparator.comparing(ActionEntity::getSort).reversed()
                            .thenComparing(ActionEntity::getId).reversed()
                    )
                    .map(actionMapper::entityToVo)
                    .collect(Collectors.toCollection(LinkedHashSet::new))
            );
            vo.setSubmenus(new HashSet<>());
          } else {
            Set<SubmenuEntityVo> submenuEntityVos =
                menuEntity.getSubmenus()
                    .stream()
                    .filter(submenuEntity -> submenuEntity.getRoles().isEmpty())
                    .sorted(
                        Comparator.comparing(SubmenuEntity::getSort).reversed()
                            .thenComparing(SubmenuEntity::getId).reversed()
                    )
                    .map(submenuEntity -> {
                      SubmenuEntityVo submenuEntityVo = submenuMapper.entityToVo(submenuEntity);
                      submenuEntityVo.setActions(
                          submenuEntity.getActions()
                              .stream()
                              .filter(actionEntity -> Objects.isNull(actionEntity.getRole()))
                              .sorted(
                                  Comparator.comparing(ActionEntity::getSort).reversed()
                                      .thenComparing(ActionEntity::getId).reversed()
                              )
                              .map(actionMapper::entityToVo)
                              .collect(Collectors.toCollection(LinkedHashSet::new))
                      );
                      return submenuEntityVo;
                    })
                    .collect(Collectors.toCollection(LinkedHashSet::new));
            vo.setActions(new HashSet<>());
            vo.setSubmenus(submenuEntityVos);
          }
          return vo;
        })
        .collect(Collectors.toCollection(LinkedHashSet::new));
  }

  /**
   * get authenticated user menu.
   *
   * @return Set
   */
  private Set<MenuEntityVo> getAuthenticatedUserMenus() {
    return userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new)
        .getRoles()
        .stream()
        .filter(roleEntity -> !roleEntity.getMenus().isEmpty())
        .flatMap(roleEntity -> roleEntity.getMenus()
            .stream()
            .map(menuEntity -> {
              MenuEntityVo vo = new MenuEntityVo();
              if (menuEntity.getSubmenus().isEmpty()) {
                vo.setActions(
                    menuEntity.getActions()
                        .stream()
                        .filter(actionEntity -> Objects.equals(actionEntity.getRole(),
                            roleEntity))
                        .sorted(
                            Comparator.comparing(ActionEntity::getSort).reversed()
                                .thenComparing(ActionEntity::getId).reversed()
                        )
                        .map(actionMapper::entityToVo)
                        .collect(Collectors.toCollection(LinkedHashSet::new))
                );
                vo.setSubmenus(new HashSet<>());
              } else {
                Set<SubmenuEntityVo> submenuEntityVos = menuEntity.getSubmenus()
                    .stream()
                    .filter(submenuEntity -> submenuEntity.getRoles().contains(roleEntity))
                    .sorted(
                        Comparator.comparing(SubmenuEntity::getSort).reversed()
                            .thenComparing(SubmenuEntity::getId).reversed()
                    )
                    .map(submenuEntity -> {
                      SubmenuEntityVo submenuEntityVo = submenuMapper.entityToVo(submenuEntity);
                      submenuEntityVo.setActions(
                          submenuEntity.getActions()
                              .stream()
                              .filter(actionEntity -> Objects.equals(actionEntity.getRole(),
                                  roleEntity))
                              .sorted(
                                  Comparator.comparing(ActionEntity::getSort).reversed()
                                      .thenComparing(ActionEntity::getId).reversed()
                              )
                              .map(actionMapper::entityToVo)
                              .collect(Collectors.toCollection(LinkedHashSet::new))
                      );
                      return submenuEntityVo;
                    })
                    .collect(Collectors.toCollection(LinkedHashSet::new));
                vo.setActions(new HashSet<>());
                vo.setSubmenus(submenuEntityVos);
              }
              return vo;
            })
        )
        .collect(Collectors.toCollection(LinkedHashSet::new));
  }
}