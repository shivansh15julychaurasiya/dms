package ahc.dms.payload.dto;

import com.fasterxml.jackson.annotation.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.time.LocalDateTime;

@NoArgsConstructor
@Getter
@Setter
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserRoleDto {

    @JsonProperty("ur_id")
    private Long urId;

    @JsonProperty("user_id")
    private Long userId;

    @JsonProperty("role_id")
    private Long roleId; // use Long to match Lookup ID

    @JsonProperty("status")
    private Boolean status; // maps to ur_rec_status: true = active, false = inactive

    // audit fields
    @JsonProperty("created_at")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    private LocalDateTime createdAt;

    @JsonProperty("updated_at")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    private LocalDateTime updatedAt;

}
