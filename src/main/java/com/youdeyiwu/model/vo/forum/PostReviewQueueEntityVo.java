package com.youdeyiwu.model.vo.forum;

import com.youdeyiwu.model.vo.AbstractEntityVo;
import com.youdeyiwu.model.vo.user.UserEntityVo;
import java.time.LocalDate;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * post review queue.
 *
 * @author dafengzhen
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class PostReviewQueueEntityVo extends AbstractEntityVo {

  private Boolean received;

  private LocalDate latestReviewResultTime;

  private UserEntityVo receiver;

  private PostEntityVo post;

}