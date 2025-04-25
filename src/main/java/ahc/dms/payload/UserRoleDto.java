package ahc.dms.payload;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@NoArgsConstructor
@Getter
@Setter
@ToString
public class UserRoleDto {

    @JsonProperty("ur_id")
    private int urId;
    @JsonProperty("user_id")
    private int userId;
    @JsonProperty("role_id")
    private int roleId;
    @JsonProperty("status")
    private boolean status;


}
