package com.youdeyiwu.model.dto.user;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import java.io.Serializable;

/**
 * users count by date.
 *
 * @param pastDays pastDays
 */
public record UsersCountByDateDto(
    @Min(1)
    @Max(90)
    Integer pastDays
) implements Serializable {

}
