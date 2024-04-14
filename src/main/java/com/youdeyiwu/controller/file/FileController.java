package com.youdeyiwu.controller.file;

import com.youdeyiwu.model.entity.file.FileEntity;
import com.youdeyiwu.model.vo.ErrorObjVo;
import com.youdeyiwu.model.vo.ErrorVo;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.file.FileEntityVo;
import com.youdeyiwu.service.file.FileService;
import java.util.List;
import java.util.concurrent.TimeUnit;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.data.web.SortDefault;
import org.springframework.http.CacheControl;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

/**
 * file.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@RequestMapping(value = "/files")
@RestController
public class FileController {

  private final FileService fileService;

  /**
   * upload image file.
   *
   * @param upload upload
   * @return ResponseEntity
   */
  @PostMapping(value = "/images", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
  public ResponseEntity<Object> uploadImageFile(MultipartFile upload) {
    try {
      return ResponseEntity.ok(fileService.uploadImageFile(upload));
    } catch (Exception e) {
      ErrorObjVo vo = new ErrorObjVo();
      vo.setError(new ErrorVo(HttpStatus.BAD_REQUEST.value(), e.getMessage()));
      return ResponseEntity.badRequest().body(vo);
    }
  }

  @GetMapping(value = "/images/users/{userId}")
  public ResponseEntity<List<FileEntityVo>> queryImageFiles(@PathVariable Long userId) {
    return ResponseEntity.ok(fileService.queryImageFiles(userId));
  }

  /**
   * access image file.
   *
   * @param id id
   * @return ResponseEntity
   */
  @GetMapping(value = "/images/{id}/{name}")
  public ResponseEntity<byte[]> queryImageFile(
      @PathVariable Long id,
      @PathVariable String name,
      @RequestHeader(value = "If-None-Match", required = false) String ifNoneMatch
  ) {
    FileEntity fileEntity = fileService.queryImageFile(id, name);
    fileService.updateViewCount(fileEntity);

    CacheControl cacheControl = CacheControl.maxAge(31, TimeUnit.DAYS).noTransform().mustRevalidate();
    String etag = "\"" + fileEntity.getDigest() + "\"";

    if (etag.equals(ifNoneMatch)) {
      return ResponseEntity.status(HttpStatus.NOT_MODIFIED)
          .eTag(etag)
          .cacheControl(cacheControl)
          .build();
    }

    return ResponseEntity.ok()
        .contentType(MediaType.valueOf(fileEntity.getContentType()))
        .contentLength(fileEntity.getFile().length)
        .eTag(etag)
        .cacheControl(cacheControl)
        .body(fileEntity.getFile());
  }

  @GetMapping
  public ResponseEntity<PageVo<FileEntityVo>> queryAll(
      @PageableDefault(size = 15)
      @SortDefault(value = {"id"}, direction = Sort.Direction.DESC)
      Pageable pageable
  ) {
    return ResponseEntity.ok(fileService.queryAll(pageable));
  }

  @GetMapping(value = "/{id}")
  public ResponseEntity<FileEntityVo> query(@PathVariable Long id) {
    return ResponseEntity.ok(fileService.query(id));
  }

  /**
   * remove image file.
   *
   * @param id id
   * @return ResponseEntity
   */
  @DeleteMapping(value = "/images/{id}")
  public ResponseEntity<Void> removeImageFile(@PathVariable Long id) {
    fileService.checkImageFile(id);
    fileService.remove(id);
    return ResponseEntity.noContent().build();
  }

  @DeleteMapping(value = "/{id}")
  public ResponseEntity<Void> remove(@PathVariable Long id) {
    fileService.remove(id);
    return ResponseEntity.noContent().build();
  }
}