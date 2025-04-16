package ahc.dms.payload;

import lombok.Data;

import java.util.List;

@Data
public class ApiResponse<T> {

    private boolean status;
    private String message;
    private T data;
    private long timestamp;

}
