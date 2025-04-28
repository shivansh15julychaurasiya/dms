package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.OtpLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface OtpLogRepository extends JpaRepository<OtpLog, Long> {
    Optional<OtpLog> findByLoginIdAndOtpType(String loginId, String otpType);
    Optional<OtpLog> findByLoginIdAndOtpTypeAndOtpValue(String loginId, String otpTypeLogin, String otp);
}
