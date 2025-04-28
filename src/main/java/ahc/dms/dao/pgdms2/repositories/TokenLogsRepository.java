package ahc.dms.dao.pgdms2.repositories;

import ahc.dms.dao.pgdms2.entities.TokenLogs;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TokenLogsRepository extends JpaRepository<TokenLogs, Long> {

    TokenLogs findByLoginId(String loginId);
    TokenLogs findByLoginIdAndJwtToken(String loginId, String token);

}
