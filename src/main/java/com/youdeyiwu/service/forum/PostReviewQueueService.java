package com.youdeyiwu.service.forum;

import com.youdeyiwu.model.dto.forum.ApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.NotApprovedPostReviewQueueDto;
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
   * @param dto dto
   */
  void receive(ReceivePostReviewQueueDto dto);

  /**
   * refund.
   *
   * @param id  id
   * @param dto dto
   */
  void refund(Long id, RefundPostReviewQueueDto dto);

  /**
   * approved.
   *
   * @param id  id
   * @param dto dto
   */
  void approved(Long id, ApprovedPostReviewQueueDto dto);

  /**
   * not approved.
   *
   * @param id  id
   * @param dto dto
   */
  void notApproved(Long id, NotApprovedPostReviewQueueDto dto);

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
}