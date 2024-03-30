package com.youdeyiwu.controller.forum;

import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.model.dto.forum.UpdateStateReplyDto;
import com.youdeyiwu.model.vo.forum.QuoteReplyEntityVo;
import com.youdeyiwu.service.forum.ReplyService;
import jakarta.validation.Valid;
import java.net.URI;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * reply.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/replies")
@RestController
public class ReplyController {

  private final ReplyService replyService;

  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateReplyDto dto) {
    return ResponseEntity.created(URI.create("/replies/" + replyService.create(dto).getId()))
        .build();
  }

  @PutMapping(value = "/{id}/state")
  public ResponseEntity<Void> updateState(
      @PathVariable Long id,
      @Valid @RequestBody UpdateStateReplyDto dto
  ) {
    replyService.updateState(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/like")
  public ResponseEntity<Void> updateLike(@PathVariable Long id) {
    replyService.updateLike(id);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<QuoteReplyEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok(replyService.query(id));
  }
}