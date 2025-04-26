package ahc.dms.payload;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.Map;

@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ApiResponse<T> {

    private boolean status;
    private String message;
    private Map<String, String> messages;
    private T data;
    private long timestamp;

}
