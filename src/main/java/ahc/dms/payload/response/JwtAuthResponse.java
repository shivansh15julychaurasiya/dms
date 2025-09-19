package ahc.dms.payload.response;

import ahc.dms.payload.dto.UserDto;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class JwtAuthResponse {

    private String token;
    private String message;
    private UserDto user;
}
