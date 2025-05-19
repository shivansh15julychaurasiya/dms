package ahc.dms.dao.dms.services;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.dms.entities.OtpLog;
import ahc.dms.dao.dms.repositories.OtpLogRepository;
import ahc.dms.payload.OtpLogDto;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class OtpLogService {

    @Autowired
    private OtpLogRepository otpLogRepository;
    @Autowired
    private ModelMapper modelMapper;

    @Transactional(transactionManager = "dmsTransactionManager")
    public OtpLogDto saveOtp(OtpLogDto otpLogDto) {
        OtpLog newOtp = otpLogRepository.save(modelMapper.map(otpLogDto, OtpLog.class));
        return modelMapper.map(newOtp, OtpLogDto.class);
    }

    public OtpLogDto getOtpLogByUsernameAndOtpType(String username, String otpType) {
        OtpLog otpLog = otpLogRepository.findByUsernameAndOtpType(username, otpType).orElse(new OtpLog());
        return modelMapper.map(otpLog, OtpLogDto.class);
    }

    public boolean verifyLoginOtp(String username, String otp) {
        return otpLogRepository.findByUsernameAndOtpTypeAndOtpValueAndOtpStatusTrue(username, AppConstants.LOGIN, otp).isPresent();
    }

    public boolean verifyResetOtp(String username, String otp) {
        return otpLogRepository.findByUsernameAndOtpTypeAndOtpValueAndOtpStatusTrue(username, AppConstants.RESET_TOKEN, otp).isPresent();
    }

    public boolean verifyForgotOtp(String username, String otp) {
        return otpLogRepository.findByUsernameAndOtpTypeAndOtpValueAndOtpStatusTrue(username, AppConstants.FORGOT_TOKEN, otp).isPresent();
    }
}
