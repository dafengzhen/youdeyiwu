package com.youdeyiwu.controller.user;

import com.youdeyiwu.model.dto.user.AssignRolesDto;
import com.youdeyiwu.model.dto.user.DisableCommentReplyUserDto;
import com.youdeyiwu.model.dto.user.LoginDto;
import com.youdeyiwu.model.dto.user.RegisterDto;
import com.youdeyiwu.model.dto.user.UpdateRolesUserDto;
import com.youdeyiwu.model.dto.user.UpdateUserPasswordDto;
import com.youdeyiwu.model.dto.user.UpdateUserProfileDto;
import com.youdeyiwu.model.dto.user.UpdateUserStatesDto;
import com.youdeyiwu.model.dto.user.UpdateUserTemporaryStorageDto;
import com.youdeyiwu.model.dto.user.UpdateUserUsernameDto;
import com.youdeyiwu.model.dto.user.UsersCountByDateDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.TokenVo;
import com.youdeyiwu.model.vo.user.MenuEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import com.youdeyiwu.model.vo.user.UserRolesPermissionsVo;
import com.youdeyiwu.model.vo.user.UsersCountByDateVo;
import com.youdeyiwu.service.config.RootConfigService;
import com.youdeyiwu.service.forum.PostService;
import com.youdeyiwu.service.user.UserService;
import jakarta.validation.Valid;
import java.util.List;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * user.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/users")
@RestController
public class UserController {

  private final UserService userService;

  private final RootConfigService rootConfigService;

  private final PostService postService;

  @PostMapping(value = "/register")
  public ResponseEntity<TokenVo> register(@Valid @RequestBody RegisterDto dto) {
    userService.checkDisableRegistration(rootConfigService.queryDisableRegistration());
    return ResponseEntity.ok(userService.register(dto));
  }

  @PostMapping(value = "/login")
  public ResponseEntity<TokenVo> login(@Valid @RequestBody LoginDto dto) {
    return ResponseEntity.ok(userService.login(dto));
  }

  @PostMapping(value = "/logout")
  public ResponseEntity<Void> logout() {
    userService.logout();
    return ResponseEntity.noContent().build();
  }

  @PostMapping("/{id}/add-roles")
  public ResponseEntity<Void> addRoles(
      @PathVariable Long id,
      @Valid @RequestBody AssignRolesDto dto
  ) {
    userService.addRoles(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PostMapping("/{id}/remove-roles")
  public ResponseEntity<Void> removeRoles(
      @PathVariable Long id,
      @Valid @RequestBody AssignRolesDto dto
  ) {
    userService.removeRoles(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/roles")
  public ResponseEntity<Void> updateRoles(
      @PathVariable Long id,
      @Valid @RequestBody UpdateRolesUserDto dto
  ) {
    userService.updateRoles(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}/profile")
  public ResponseEntity<Void> updateProfile(
      @PathVariable Long id,
      @Valid @RequestBody UpdateUserProfileDto dto
  ) {
    userService.updateProfile(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}/username")
  public ResponseEntity<Void> updateUsername(
      @PathVariable Long id,
      @Valid @RequestBody UpdateUserUsernameDto dto
  ) {
    userService.updateUsername(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}/password")
  public ResponseEntity<Void> updatePassword(
      @PathVariable Long id,
      @Valid @RequestBody UpdateUserPasswordDto dto
  ) {
    userService.updatePassword(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}/states")
  public ResponseEntity<Void> updateStates(
      @PathVariable Long id,
      @Valid @RequestBody UpdateUserStatesDto dto
  ) {
    userService.updateStates(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/temporary-storage")
  public ResponseEntity<Void> updateTemporaryStorage(@Valid @RequestBody UpdateUserTemporaryStorageDto dto) {
    userService.updateTemporaryStorage(dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/disable-comment-reply")
  public ResponseEntity<Void> disableCommentReply(
      @PathVariable Long id,
      @Valid @RequestBody DisableCommentReplyUserDto dto
  ) {
    userService.disableCommentReply(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/temporary-storage")
  public ResponseEntity<String> queryTemporaryStorage() {
    return ResponseEntity.ok(userService.queryTemporaryStorage());
  }

  @GetMapping("/{id}/roles-permissions")
  public ResponseEntity<UserRolesPermissionsVo> getRolesPermissions(@PathVariable Long id) {
    return ResponseEntity.ok(userService.getRolesPermissions(id));
  }

  @GetMapping("/menus")
  public ResponseEntity<List<MenuEntityVo>> getMenus() {
    return ResponseEntity.ok(userService.getMenus());
  }

  @GetMapping("/login-info")
  public ResponseEntity<UserEntityVo> getLoginInfo() {
    return ResponseEntity.ok(userService.getLoginInfo());
  }

  @GetMapping("/count-by-date")
  public ResponseEntity<List<UsersCountByDateVo>> getUsersCountByDate(
      @Valid UsersCountByDateDto dto
  ) {
    return ResponseEntity.ok(userService.getUsersCountByDate(dto));
  }

  @GetMapping(value = "/select-all")
  public ResponseEntity<PageVo<UserEntityVo>> selectAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok(userService.selectAll(pageable));
  }

  /**
   * query details.
   *
   * @param id id
   * @return ResponseEntity
   */
  @GetMapping(value = "/{id}/details")
  public ResponseEntity<UserEntityVo> queryDetails(@PathVariable String id) {
    if (id.contains(".")) {
      return ResponseEntity.badRequest().build();
    }

    UserEntityVo vo = userService.queryDetails(id);
    if (!CollectionUtils.isEmpty(vo.getPosts())) {
      vo.setPosts(
          vo.getPosts()
              .stream()
              .map(postEntityVo -> postService.queryDetails(
                  PageRequest.of(0, 1),
                  postEntityVo.getId(),
                  null)
              )
              .collect(Collectors.toSet())
      );
    }

    return ResponseEntity.ok(vo);
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<UserEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok(userService.query(id));
  }

  @GetMapping
  public ResponseEntity<PageVo<UserEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok(userService.queryAll(pageable));
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    userService.delete(id);
    return ResponseEntity.noContent().build();
  }
}