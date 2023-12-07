package com.youdeyiwu.exception;

import com.youdeyiwu.model.vo.ErrorVo;
import java.util.Objects;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

/**
 * global exception handler.
 *
 * @author dafengzhen
 */
@RestControllerAdvice
public class GlobalExceptionHandler {

  /**
   * custom exception.
   *
   * @param exception exception
   * @return ResponseEntity
   */
  @ExceptionHandler(CustomException.class)
  public ResponseEntity<ErrorVo> handleCustomException(CustomException exception) {
    return ResponseEntity.badRequest()
        .body(new ErrorVo(HttpStatus.BAD_REQUEST.value(), exception.getMessage()));
  }

  /**
   * bind exception.
   *
   * @param exception exception
   * @return ResponseVO
   */
  @ExceptionHandler(BindException.class)
  public ResponseEntity<ErrorVo> handleBindException(BindException exception) {
    ErrorVo vo = new ErrorVo(
        HttpStatus.BAD_REQUEST.value(),
        HttpStatus.BAD_REQUEST.getReasonPhrase()
    );
    FieldError fieldError = exception.getFieldError();
    if (Objects.nonNull(fieldError)) {
      vo.setMessage(fieldError.getDefaultMessage());
    }
    return ResponseEntity.badRequest().body(vo);
  }
}
