package com.youdeyiwu.controller.forum;

import com.youdeyiwu.model.dto.forum.CreateReplyDto;
import com.youdeyiwu.service.forum.ReplyService;
import jakarta.validation.Valid;
import java.net.URI;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
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
}