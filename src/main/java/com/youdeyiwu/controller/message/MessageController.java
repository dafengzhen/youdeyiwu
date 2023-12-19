package com.youdeyiwu.controller.message;

import com.youdeyiwu.model.dto.message.CreateGlobalMessageDto;
import com.youdeyiwu.model.dto.message.CreateMessageDto;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.message.GlobalMessageEntityVo;
import com.youdeyiwu.model.vo.message.MessageEntityVo;
import com.youdeyiwu.service.message.MessageService;
import jakarta.validation.Valid;
import java.net.URI;
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
import org.springframework.web.bind.annotation.RestController;

/**
 * message.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/messages")
@RestController
public class MessageController {

  private final MessageService messageService;

  /**
   * create global message.
   *
   * @param dto dto
   * @return ResponseEntity
   */
  @PostMapping(value = "/global-messages")
  public ResponseEntity<Void> createGlobalMessage(@Valid @RequestBody CreateGlobalMessageDto dto) {
    return ResponseEntity
        .created(
            URI.create(
                "/messages/global-messages" + messageService.createGlobalMessage(dto).getId())
        )
        .build();
  }

  /**
   * create.
   *
   * @param dto dto
   * @return ResponseEntity
   */
  @PostMapping
  public ResponseEntity<Void> create(@Valid @RequestBody CreateMessageDto dto) {
    return ResponseEntity
        .created(URI.create("/messages/" + messageService.create(dto).getId()))
        .build();
  }

  @PutMapping("/global-messages/{id}/state")
  public ResponseEntity<Void> updateGlobalMessageState(@PathVariable Long id) {
    messageService.updateGlobalMessageState(id);
    return ResponseEntity.noContent().build();
  }

  @PutMapping("/{id}/state")
  public ResponseEntity<Void> updateState(@PathVariable Long id) {
    messageService.updateState(id);
    return ResponseEntity.noContent().build();
  }

  @GetMapping(value = "/global-messages/{id}")
  public ResponseEntity<GlobalMessageEntityVo> queryGlobalMessage(@PathVariable Long id) {
    return ResponseEntity.ok().body(messageService.queryGlobalMessage(id));
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<MessageEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok().body(messageService.query(id));
  }

  @GetMapping(value = "/global-messages")
  public ResponseEntity<PageVo<GlobalMessageEntityVo>> queryAllGlobalMessage(
      @PageableDefault(size = 15)
      @SortDefault(value = {"sort", "id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok().body(messageService.queryAllGlobalMessage(pageable));
  }

  @GetMapping
  public ResponseEntity<PageVo<MessageEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault.SortDefaults(value = {
          @SortDefault(value = "state"),
          @SortDefault(value = "id", direction = Sort.Direction.DESC)
      })
      Pageable pageable
  ) {
    return ResponseEntity.ok().body(messageService.queryAll(pageable));
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> delete(@PathVariable Long id) {
    messageService.delete(id);
    return ResponseEntity.noContent().build();
  }
}