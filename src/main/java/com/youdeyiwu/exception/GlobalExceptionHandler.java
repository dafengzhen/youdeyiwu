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
    String message = exception.getMessage();
    int status = 400;
    if (HttpStatus.UNAUTHORIZED.getReasonPhrase().equals(message)) {
      status = HttpStatus.UNAUTHORIZED.value();
    } else if (HttpStatus.FORBIDDEN.getReasonPhrase().equals(message)) {
      status = HttpStatus.FORBIDDEN.value();
    } else if (HttpStatus.NOT_FOUND.getReasonPhrase().equals(message)) {
      status = HttpStatus.NOT_FOUND.value();
    } else if (HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase().equals(message)) {
      status = HttpStatus.INTERNAL_SERVER_ERROR.value();
    }
    return ResponseEntity.status(status).body(new ErrorVo(status, message));
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
