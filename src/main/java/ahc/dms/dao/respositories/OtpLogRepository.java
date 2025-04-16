package ahc.dms.dao.respositories;

import ahc.dms.dao.entities.OtpLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface OtpLogRepository extends JpaRepository<OtpLog, Long> {
    OtpLog findByLoginId(String loginId);
}
