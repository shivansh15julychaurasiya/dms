package ahc.dms.dao.pgdms.repositories;

import ahc.dms.dao.pgdms.entities.RequestLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface RequestLogRepository extends JpaRepository<RequestLog, Long> {
}