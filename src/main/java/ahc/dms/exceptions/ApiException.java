package ahc.dms.exceptions;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ApiException extends RuntimeException {
	
//  ********************** Fullstack Java Developer Vijay Chaurasiya *******************************


    public ApiException(String message) {
        super(message);
    }

}
