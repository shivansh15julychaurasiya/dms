package ahc.dms.dao.services;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.entities.OtpLog;
import ahc.dms.dao.respositories.OtpLogRepository;
import ahc.dms.payload.OtpDto;
import jakarta.transaction.Transactional;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class OtpLogService {

    @Autowired
    private OtpLogRepository otpLogRepository;
    @Autowired
    private ModelMapper modelMapper;

    @Transactional
    public OtpDto saveOtp(OtpDto otpDto) {
        OtpLog newOtp = otpLogRepository.save(modelMapper.map(otpDto, OtpLog.class));
        return modelMapper.map(newOtp, OtpDto.class);
    }

    public OtpDto getOtpLogByLoginIdAndOtpType(String loginId, String otpType) {
        OtpLog otpLog = otpLogRepository.findByLoginIdAndOtpType(loginId, otpType).orElse(new OtpLog());
        return modelMapper.map(otpLog, OtpDto.class);
    }

    public boolean verifyLoginOtp(String loginId, String otp) {
        return otpLogRepository.findByLoginIdAndOtpTypeAndOtpValue(loginId, AppConstants.OTP_TYPE_LOGIN, otp).isPresent();
    }
}
