package ahc.dms.payload;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class JwtAuthResponse {

    private String token;
    private String message;
    private UserDto user;
}
