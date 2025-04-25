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

    @JsonProperty("user_id")
    private int userId;
    @JsonProperty("role_id")
    private int roleId;
    @JsonProperty("role_name")
    private String roleName; // Optional, for convenience
    private boolean status;

}
