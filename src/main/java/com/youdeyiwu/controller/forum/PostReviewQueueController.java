package com.youdeyiwu.controller.forum;

import com.youdeyiwu.model.dto.forum.ApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.NotApprovedPostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.ReceivePostReviewQueueDto;
import com.youdeyiwu.model.dto.forum.RefundPostReviewQueueDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.model.vo.forum.PostReviewQueueEntityVo;
import com.youdeyiwu.service.forum.PostReviewQueueService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * post review queue.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/posts/review-queues")
@RestController
public class PostReviewQueueController {

  private final PostReviewQueueService postReviewQueueService;

  @PostMapping(value = "/receive")
  public ResponseEntity<Void> receive(@Valid @RequestBody ReceivePostReviewQueueDto dto) {
    postReviewQueueService.receive(dto);
    return ResponseEntity.noContent().build();
  }

  @PostMapping(value = "/{id}/refund")
  public ResponseEntity<Void> refund(
      @PathVariable Long id,
      @Valid @RequestBody RefundPostReviewQueueDto dto
  ) {
    postReviewQueueService.refund(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PostMapping(value = "/{id}/approved")
  public ResponseEntity<Void> approved(
      @PathVariable Long id,
      @Valid @RequestBody ApprovedPostReviewQueueDto dto
  ) {
    postReviewQueueService.approved(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PostMapping(value = "/{id}/not-approved")
  public ResponseEntity<Void> notApproved(
      @PathVariable Long id,
      @Valid @RequestBody NotApprovedPostReviewQueueDto dto
  ) {
    postReviewQueueService.notApproved(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<PostReviewQueueEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok().body(postReviewQueueService.query(id));
  }

  @GetMapping
  public ResponseEntity<PageVo<PostEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok().body(postReviewQueueService.queryAll(pageable));
  }
}