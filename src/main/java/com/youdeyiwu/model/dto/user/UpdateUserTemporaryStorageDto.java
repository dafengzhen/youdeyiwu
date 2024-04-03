package com.youdeyiwu.model.dto.user;

import java.io.Serializable;

/**
 * update user temporary storage.
 *
 * @param temporaryStorage temporaryStorage
 */
public record UpdateUserTemporaryStorageDto(
    String temporaryStorage
) implements Serializable {

}
