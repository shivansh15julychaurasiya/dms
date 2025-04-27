package ahc.dms.payload;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.time.LocalDateTime;

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

    // audit fields
    @JsonProperty("created_at")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    private LocalDateTime createdAt;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}
