package com.youdeyiwu.repository.user;

import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.user.UsersCountByDateVo;
import java.time.OffsetDateTime;
import java.util.List;

/**
 * user.
 *
 * @author dafengzhen
 */
public interface CustomizedUserRepository {

  /**
   * get users count by date.
   *
   * @param startDate startDate
   * @param endDate   endDate
   * @return List
   */
  List<UsersCountByDateVo> getUsersCountByDate(
      OffsetDateTime startDate,
      OffsetDateTime endDate
  );

  /**
   * find all role by id.
   *
   * @param id id
   * @return UserEntity
   */
  UserEntity findAllRoleById(Long id);
}
