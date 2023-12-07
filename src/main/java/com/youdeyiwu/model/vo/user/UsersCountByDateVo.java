package com.youdeyiwu.model.vo.user;

import java.time.LocalDate;
import lombok.Data;

/**
 * users count by date.
 *
 * @author dafengzhen
 */
@Data
public class UsersCountByDateVo {

  /**
   * date.
   */
  private LocalDate date;

  /**
   * count.
   */
  private Long count;

}