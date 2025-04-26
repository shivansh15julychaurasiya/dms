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
    private Long urId;
    @JsonProperty("user_id")
    private Long userId;
    @JsonProperty("role_id")
    private Integer roleId;
    @JsonProperty("status")
    private Boolean status;


}
