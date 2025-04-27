package ahc.dms.payload;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OtpDto {

    private Long otpId;
    @JsonProperty("login_id")
    private String loginId;
    @JsonProperty("otp_type")
    private String otpType;
    private String phone;
    private String otpValue;
    private LocalDateTime otpExpiry;
    private Boolean otpStatus;
    private String status;
    private String message;
    @JsonProperty("sms_id")
    private String smsId;

    // audit fields
    @JsonProperty("created_at")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    private LocalDateTime createdAt;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd-MM-yyyy HH:mm:ss")
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;

}
