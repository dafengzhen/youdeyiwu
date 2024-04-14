package com.youdeyiwu.service.file.impl;

import static org.springframework.http.MediaType.IMAGE_GIF_VALUE;
import static org.springframework.http.MediaType.IMAGE_JPEG_VALUE;
import static org.springframework.http.MediaType.IMAGE_PNG_VALUE;

import com.youdeyiwu.enums.file.FileCategoryEnum;
import com.youdeyiwu.exception.CustomException;
import com.youdeyiwu.exception.FileNotFoundException;
import com.youdeyiwu.exception.UserNotFoundException;
import com.youdeyiwu.mapper.file.FileMapper;
import com.youdeyiwu.mapper.user.UserMapper;
import com.youdeyiwu.model.entity.file.FileEntity;
import com.youdeyiwu.model.entity.user.UserEntity;
import com.youdeyiwu.model.vo.PageVo;
import com.youdeyiwu.model.vo.file.FileEntityVo;
import com.youdeyiwu.model.vo.file.FileUrlsVo;
import com.youdeyiwu.repository.file.FileRepository;
import com.youdeyiwu.repository.user.UserRepository;
import com.youdeyiwu.security.SecurityService;
import com.youdeyiwu.service.file.FileService;
import com.youdeyiwu.tool.I18nTool;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.tika.Tika;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.DigestUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

/**
 * file.
 *
 * @author dafengzhen
 */
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Service
public class FileServiceImpl implements FileService {

  private final FileRepository fileRepository;

  private final UserRepository userRepository;

  private final FileMapper fileMapper;

  private final SecurityService securityService;

  private final Tika tika = new Tika();

  private final I18nTool i18nTool;

  private final UserMapper userMapper;

  private static final int BYTES_PER_KB = 1024;

  private static final int MAX_FILE_SIZE_KB = 500;

  @Transactional
  @Override
  public void updateViewCount(FileEntity fileEntity) {
    fileEntity.setViewCount(fileEntity.getViewCount() + 1);
    fileRepository.save(fileEntity);
  }

  @Transactional
  @Override
  public FileUrlsVo uploadImageFile(MultipartFile file) {
    if (file.isEmpty() || !StringUtils.hasText(file.getContentType())) {
      throw new CustomException(i18nTool.getMessage("file.contentType.required"));
    }

    if (file.getSize() > (MAX_FILE_SIZE_KB * BYTES_PER_KB)) {
      throw new CustomException(i18nTool.getMessage("file.size.limit", Map.of("size", MAX_FILE_SIZE_KB)));
    }

    String mediaType;
    byte[] bytes;
    try {
      mediaType = tika.detect(file.getInputStream(), file.getOriginalFilename());
      bytes = file.getBytes();
    } catch (IOException e) {
      throw new CustomException(e.getMessage());
    }

    if (
        !StringUtils.hasText(mediaType)
            || (
            !mediaType.startsWith(IMAGE_JPEG_VALUE)
                && !mediaType.startsWith(IMAGE_PNG_VALUE)
                && !mediaType.startsWith(IMAGE_GIF_VALUE)
        )
    ) {
      throw new CustomException(i18nTool.getMessage("file.unsupportedType"));
    }

    String digest = DigestUtils.md5DigestAsHex(bytes);
    FileEntity entity = fileRepository.findByDigest(digest)
        .orElseGet(() -> createNewFileEntity(file, mediaType, bytes, digest));
    entity.setUrl("/files/images/" + entity.getId() + "/" + entity.getName());
    FileUrlsVo vo = new FileUrlsVo();
    vo.setUrl("/api" + entity.getUrl());
    return vo;
  }

  @Override
  public List<FileEntityVo> queryImageFiles(Long userId) {
    UserEntity userEntity = userRepository.findById(userId)
        .orElseThrow(UserNotFoundException::new);
    return userEntity.getFiles()
        .stream()
        .filter(fileEntity -> fileEntity.getFileCategory() == FileCategoryEnum.IMAGE)
        .map(fileMapper::entityToVo)
        .toList();
  }

  @Transactional
  @Override
  public FileEntity queryImageFile(Long id, String name) {
    FileEntity fileEntity = fileRepository.findById(id)
        .orElseThrow(FileNotFoundException::new);

    if (
        !Objects.equals(fileEntity.getName(), name)
            || fileEntity.getFileCategory() != FileCategoryEnum.IMAGE
    ) {
      throw new CustomException(i18nTool.getMessage("file.notFound"));
    }

    return fileEntity;
  }

  @Override
  public void checkImageFile(Long id) {
    FileEntity fileEntity = fileRepository.findById(id)
        .orElseThrow(FileNotFoundException::new);
    UserEntity userEntity = userRepository.findById(securityService.getUserId())
        .orElseThrow(UserNotFoundException::new);

    if (!userEntity.getFiles().contains(fileEntity)) {
      throw new CustomException(i18nTool.getMessage("file.notFound"));
    }
  }

  @Override
  public PageVo<FileEntityVo> queryAll(Pageable pageable) {
    return new PageVo<>(fileRepository.findAll(pageable).map(fileEntity -> {
      FileEntityVo vo = fileMapper.entityToVo(fileEntity);
      vo.setUser(userMapper.entityToVo(fileEntity.getUser()));
      return vo;
    }));
  }

  @Override
  public FileEntityVo query(Long id) {
    FileEntity fileEntity = fileRepository.findById(id)
        .orElseThrow(FileNotFoundException::new);
    FileEntityVo vo = fileMapper.entityToVo(fileEntity);
    vo.setUser(userMapper.entityToVo(fileEntity.getUser()));
    return vo;
  }

  @Transactional
  @Override
  public void remove(Long id) {
    FileEntity fileEntity = fileRepository.findById(id)
        .orElseThrow(FileNotFoundException::new);

    if (securityService.isAuthenticated()) {
      UserEntity userEntity = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      fileEntity.setUser(null);
      userEntity.getFiles().remove(fileEntity);
    } else {
      fileRepository.delete(fileEntity);
    }
  }

  /**
   * create new file entity.
   *
   * @param file      file
   * @param mediaType mediaType
   * @param bytes     bytes
   * @param digest    digest
   * @return FileEntity
   */
  private FileEntity createNewFileEntity(
      MultipartFile file,
      String mediaType,
      byte[] bytes,
      String digest
  ) {
    FileEntity fileEntity = new FileEntity();
    fileEntity.setName(RandomStringUtils.randomAlphanumeric(12));
    fileEntity.setOriginalName(file.getOriginalFilename());
    fileEntity.setDigest(digest);
    fileEntity.setMediaType(mediaType);
    fileEntity.setContentType(file.getContentType());
    fileEntity.setSize(file.getSize());
    fileEntity.setFile(bytes);

    if (securityService.isAuthenticated()) {
      UserEntity userEntity = userRepository.findById(securityService.getUserId())
          .orElseThrow(UserNotFoundException::new);
      userEntity.getFiles().add(fileEntity);
      fileEntity.setUser(userEntity);
      fileRepository.flush();
      return fileEntity;
    }

    return fileRepository.save(fileEntity);
  }
}