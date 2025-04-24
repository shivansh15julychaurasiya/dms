package ahc.dms.payload;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.Column;
import jakarta.validation.constraints.NotBlank;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
@Setter
public class RoleDto {
    private int roleId;
    @NotBlank
    @JsonProperty("role_name")
    private String roleName;
    private boolean status;
}
