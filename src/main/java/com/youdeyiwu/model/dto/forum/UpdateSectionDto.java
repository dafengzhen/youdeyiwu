package com.youdeyiwu.model.dto.forum;

import jakarta.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import org.hibernate.validator.constraints.Length;
import org.springframework.web.multipart.MultipartFile;

/**
 * update section.
 *
 * @param name       name
 * @param cover      cover
 * @param coverImage coverImage
 * @param overview   overview
 * @param content    content
 * @param sort       sort
 */
public record UpdateSectionDto(
    @Length(min = 1)
    String name,

    // @URL(regexp = "^(http|https).*")
    String cover,

    MultipartFile coverImage,

    String overview,

    String content,

    @PositiveOrZero
    Integer sort
) implements Serializable {

}
