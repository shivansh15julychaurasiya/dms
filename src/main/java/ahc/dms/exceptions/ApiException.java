package ahc.dms.exceptions;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ApiException extends RuntimeException {

    public ApiException() {
        super();
    }
    public ApiException(String message) {
        super(message);
    }

}
