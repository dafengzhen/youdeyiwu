package com.youdeyiwu.controller.forum;

import com.youdeyiwu.model.dto.forum.CreateCommentDto;
import com.youdeyiwu.model.dto.forum.UpdateStateCommentDto;
import com.youdeyiwu.model.vo.forum.CommentEntityVo;
import com.youdeyiwu.service.forum.CommentService;
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

  @PutMapping(value = "/{id}/state")
  public ResponseEntity<Void> updateState(
      @PathVariable Long id,
      @Valid @RequestBody UpdateStateCommentDto dto
  ) {
    commentService.updateState(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/like")
  public ResponseEntity<Void> updateLike(@PathVariable Long id) {
    commentService.updateLike(id);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<CommentEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok(commentService.query(id));
  }
}