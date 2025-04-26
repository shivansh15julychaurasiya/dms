package ahc.dms.payload;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotNull;
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
    private Integer urId;
    @JsonProperty("user_id")
    private Integer userId;
    @JsonProperty("role_id")
    @NotNull
    private Integer roleId;
    @JsonProperty("status")
    private Boolean status;


}
