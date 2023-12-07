package com.youdeyiwu.controller.forum;

import com.youdeyiwu.model.dto.forum.CreateCommentDto;
import com.youdeyiwu.service.forum.CommentService;
import jakarta.validation.Valid;
import java.net.URI;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * comment.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/comments")
@RestController
public class CommentController {

  private final CommentService commentService;

  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateCommentDto dto) {
    return ResponseEntity.created(URI.create("/comments/" + commentService.create(dto).getId()))
        .build();
  }
}