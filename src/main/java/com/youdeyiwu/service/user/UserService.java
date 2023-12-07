package com.youdeyiwu.service.user;

import com.youdeyiwu.model.dto.user.AssignRolesDto;
import com.youdeyiwu.model.dto.user.LoginDto;
import com.youdeyiwu.model.dto.user.RegisterDto;
import com.youdeyiwu.model.dto.user.UpdateRolesUserDto;
import com.youdeyiwu.model.dto.user.UpdateUserPasswordDto;
import com.youdeyiwu.model.dto.user.UpdateUserProfileDto;
import com.youdeyiwu.model.dto.user.UpdateUserStatesDto;
import com.youdeyiwu.model.dto.user.UpdateUserUsernameDto;
import com.youdeyiwu.model.dto.user.UsersCountByDateDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.TokenVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import com.youdeyiwu.model.vo.user.UserRolesPermissionsVo;
import com.youdeyiwu.model.vo.user.UsersCountByDateVo;
import java.util.List;
import org.springframework.data.domain.Pageable;

/**
 * user.
 *
 * @author dafengzhen
 */
public interface UserService {

  /**
   * register.
   *
   * @param dto dto
   * @return TokenVo
   */
  TokenVo register(RegisterDto dto);

  /**
   * login.
   *
   * @param dto dto
   * @return TokenVo
   */
  TokenVo login(LoginDto dto);

  /**
   * addRoles.
   *
   * @param id  id
   * @param dto dto
   */
  void addRoles(Long id, AssignRolesDto dto);

  /**
   * removeRoles.
   *
   * @param id  id
   * @param dto dto
   */
  void removeRoles(Long id, AssignRolesDto dto);

  /**
   * update roles (The difference with this method is that it updates the entire roles list).
   *
   * @param id  id
   * @param dto dto
   */
  void updateRoles(Long id, UpdateRolesUserDto dto);

  /**
   * getRolesPermissions.
   *
   * @param id id
   * @return UserRolesPermissionsVo
   */
  UserRolesPermissionsVo getRolesPermissions(Long id);

  /**
   * updateProfile.
   *
   * @param id  id
   * @param dto dto
   */
  void updateProfile(Long id, UpdateUserProfileDto dto);

  /**
   * updateUsername.
   *
   * @param id  id
   * @param dto dto
   */
  void updateUsername(Long id, UpdateUserUsernameDto dto);

  /**
   * updatePassword.
   *
   * @param id  id
   * @param dto dto
   */
  void updatePassword(Long id, UpdateUserPasswordDto dto);

  /**
   * update states.
   *
   * @param id  id
   * @param dto dto
   */
  void updateStates(Long id, UpdateUserStatesDto dto);

  /**
   * get login info.
   *
   * @return UserEntityVo
   */
  UserEntityVo getLoginInfo();

  /**
   * users count by date.
   *
   * @param dto dto
   * @return List
   */
  List<UsersCountByDateVo> getUsersCountByDate(UsersCountByDateDto dto);

  /**
   * select all.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<UserEntityVo> selectAll(Pageable pageable);

  /**
   * query details.
   *
   * @param id id
   * @return UserEntityVo
   */
  UserEntityVo queryDetails(Long id);

  /**
   * query.
   *
   * @param id id
   * @return UserEntityVo
   */
  UserEntityVo query(Long id);

  /**
   * query all.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<UserEntityVo> queryAll(Pageable pageable);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}