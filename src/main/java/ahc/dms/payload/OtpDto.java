package ahc.dms.payload;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class OtpDto {

    private Long otpId;
    private String loginId;
    private LocalDateTime otpExpiry;
    private boolean otpStatus;

}
