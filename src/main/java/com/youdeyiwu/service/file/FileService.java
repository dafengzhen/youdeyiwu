package com.youdeyiwu.service.file;

import com.youdeyiwu.model.entity.file.FileEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.file.FileEntityVo;
import com.youdeyiwu.model.vo.file.FileUrlsVo;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

/**
 * file.
 *
 * @author dafengzhen
 */
public interface FileService {

  /**
   * upload view count.
   *
   * @param fileEntity fileEntity
   */
  void updateViewCount(FileEntity fileEntity);

  /**
   * upload image file.
   *
   * @param file file
   * @return FileUrlsVo
   */
  FileUrlsVo uploadImageFile(MultipartFile file);

  /**
   * query logged in files.
   *
   * @param userId userId
   * @return List
   */
  List<FileEntityVo> queryImageFiles(Long userId);

  /**
   * query image file.
   *
   * @param id   id.
   * @param name name
   * @return FileEntity
   */
  FileEntity queryImageFile(Long id, String name);

  /**
   * check image file.
   *
   * @param id id.
   */
  void checkImageFile(Long id);

  /**
   * query all.
   *
   * @param pageable pageable
   * @return PageVo
   */
  PageVo<FileEntityVo> queryAll(Pageable pageable);

  /**
   * query.
   *
   * @param id id.
   * @return FileEntityVo
   */
  FileEntityVo query(Long id);

  /**
   * remove.
   *
   * @param id id.
   */
  void remove(Long id);
}