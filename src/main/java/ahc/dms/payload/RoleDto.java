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
public class RoleDto {
    @JsonProperty("role_id")
    private int roleId;
    @JsonProperty("role_name")
    private String roleName;
    private boolean status;
}
