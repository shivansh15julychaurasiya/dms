package ahc.dms.dao.respositories;

import ahc.dms.dao.entities.OtpLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface OtpLogRepository extends JpaRepository<OtpLog, Long> {
    Optional<OtpLog> findByLoginIdAndOtpType(String loginId, String otpType);
}
