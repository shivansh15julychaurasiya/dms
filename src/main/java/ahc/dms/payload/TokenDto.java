package ahc.dms.payload;

import com.fasterxml.jackson.annotation.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.Date;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
public class TokenDto {

    @JsonIgnore
    private Long version;
    private Long tokenId;
    private String jwtToken;
    private String loginId;
    private Date expirationDate;
    private Boolean tokenStatus;
    @JsonIgnore
    @JsonProperty("created_by")
    private String createdBy;
    @JsonIgnore
    @JsonProperty("updated_by")
    private String updatedBy;

    // audit fields
    @JsonProperty("created_at")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    private LocalDateTime createdAt;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;

}
