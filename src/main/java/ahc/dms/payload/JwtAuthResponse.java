package ahc.dms.payload;

import lombok.Data;

@Data
public class JwtAuthResponse {

    private String token;
    private String message;
    private UserDto user;
}
