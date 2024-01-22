package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.ReceivePostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.RefundPostReviewQueueDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.PostReviewQueueEntityVo;
import org.springframework.data.domain.Pageable;

/**
 * post review queue.
 *
 * @author dafengzhen
 */
public interface PostReviewQueueService {

  /**
   * receive.
   *
   * @param id  id
   * @param dto dto
   */
  void receive(Long id, ReceivePostReviewQueueDto dto);

  /**
   * refund.
   *
   * @param id  id
   * @param dto dto
   */
  void refund(Long id, RefundPostReviewQueueDto dto);

  /**
   * query.
   *
   * @param id id
   * @return PostReviewQueueEntityVo
   */
  PostReviewQueueEntityVo query(Long id);

  /**
   * queryAll.
   *
   * @param pageable pageable
   * @return PostReviewQueueEntityVo
   */
  PageVo<PostReviewQueueEntityVo> queryAll(Pageable pageable);

  /**
   * delete.
   *
   * @param id id
   */
  void delete(Long id);
}