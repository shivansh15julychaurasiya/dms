package ahc.dms.dao._payloads;

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
    private String username;
    private Date expirationDate;
    private boolean tokenStatus;

}
