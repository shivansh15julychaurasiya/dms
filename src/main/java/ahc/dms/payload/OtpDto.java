package ahc.dms.payload;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;

@Data
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
    private boolean otpStatus;
    private String status;
    private String message;
    @JsonProperty("sms_id")
    private String smsId;

}
