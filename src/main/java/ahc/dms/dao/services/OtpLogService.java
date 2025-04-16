package ahc.dms.dao.services;

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

    public OtpDto findOtpByLoginId(String loginId) {
        return modelMapper.map(otpLogRepository.findByLoginId(loginId), OtpDto.class);
    }

}
