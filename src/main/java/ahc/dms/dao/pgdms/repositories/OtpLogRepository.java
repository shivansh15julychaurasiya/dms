package ahc.dms.dao.pgdms.repositories;

import ahc.dms.dao.pgdms.entities.OtpLog;
import jakarta.persistence.PersistenceContext;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface OtpLogRepository extends JpaRepository<OtpLog, Long> {
    Optional<OtpLog> findByLoginIdAndOtpType(String loginId, String otpType);
    Optional<OtpLog> findByLoginIdAndOtpTypeAndOtpValue(String loginId, String otpTypeLogin, String otp);
}
