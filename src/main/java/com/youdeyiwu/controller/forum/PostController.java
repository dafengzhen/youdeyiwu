package com.youdeyiwu.controller.forum;

import com.google.common.net.InetAddresses;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.model.dto.forum.CreatePostDto;
import com.youdeyiwu.model.dto.forum.QueryParamsPostDto;
import com.youdeyiwu.model.dto.forum.UpdatePostDto;
import com.youdeyiwu.model.dto.forum.UpdateTagsPostDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.forum.CommentReplyVo;
import com.youdeyiwu.model.vo.forum.PostEntityVo;
import com.youdeyiwu.service.forum.PostService;
import jakarta.validation.Valid;
import java.net.URI;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.data.web.SortDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * post.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/posts")
@RestController
public class PostController {

  private final PostService postService;

  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreatePostDto dto) {
    return ResponseEntity.created(URI.create("/posts/" + postService.create(dto).getId()))
        .build();
  }

  /**
   * view page.
   *
   * @param id id
   * @param ip ip
   * @return ResponseEntity
   */
  @PostMapping("/{id}/view-page")
  public ResponseEntity<Void> viewPage(@PathVariable Long id, @RequestParam String ip) {
    if (!InetAddresses.isInetAddress(ip)) {
      throw new CustomException("Invalid IP address. Please provide a valid IP address");
    }

    postService.viewPage(id);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/like")
  public ResponseEntity<Void> updateLike(@PathVariable Long id) {
    postService.updateLike(id);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/favorite")
  public ResponseEntity<Void> updateFavorite(@PathVariable Long id) {
    postService.updateFavorite(id);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}/tags")
  public ResponseEntity<Void> updateTags(
      @PathVariable Long id,
      @Valid @RequestBody UpdateTagsPostDto dto
  ) {
    postService.updateTags(id, dto);
    return ResponseEntity.noContent().build();
  }

  @PutMapping(value = "/{id}")
  public ResponseEntity<Void> update(
      @PathVariable Long id,
      @Valid @RequestBody UpdatePostDto dto
  ) {
    postService.update(id, dto);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/random")
  public ResponseEntity<Set<PostEntityVo>> queryRandom() {
    return ResponseEntity.ok(postService.queryRandom());
  }

  @GetMapping(value = "/select-all")
  public ResponseEntity<PageVo<PostEntityVo>> selectAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"initialScore", "sortState", "id"}, direction = Sort.Direction.DESC)
      Pageable pageable,
      @Valid QueryParamsPostDto dto
  ) {
    return ResponseEntity.ok().body(postService.selectAll(pageable, dto));
  }

  @GetMapping(value = "/{id}/comment-reply")
  public ResponseEntity<PageVo<CommentReplyVo>> queryCommentReply(
      @PageableDefault(size = 15)
      @SortDefault(value = {"id"}, direction = Sort.Direction.ASC)
      Pageable pageable,
      @PathVariable Long id
  ) {
    return ResponseEntity.ok().body(postService.queryCommentReply(pageable, id));
  }

  @GetMapping(value = "/{id}/details")
  public ResponseEntity<PostEntityVo> queryDetails(
      @PageableDefault(size = 15)
      @SortDefault(value = {"id"}, direction = Sort.Direction.ASC)
      Pageable pageable,
      @PathVariable Long id
  ) {
    return ResponseEntity.ok().body(postService.queryDetails(pageable, id));
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<PostEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok().body(postService.query(id));
  }

  @GetMapping
  public ResponseEntity<PageVo<PostEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"initialScore", "sortState", "id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok().body(postService.queryAll(pageable));
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    postService.delete(id);
    return ResponseEntity.noContent().build();
  }
}