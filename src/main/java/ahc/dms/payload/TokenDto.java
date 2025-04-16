package ahc.dms.payload;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.Date;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class TokenDto {

    private Long tokenId;
    private String jwtToken;
    private String loginId;
    private Date expirationDate;
    private boolean tokenStatus;

}
