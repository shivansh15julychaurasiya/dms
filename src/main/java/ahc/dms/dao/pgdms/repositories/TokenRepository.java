package ahc.dms.dao.pgdms.repositories;

import ahc.dms.dao.pgdms.entities.Token;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TokenRepository extends JpaRepository<Token, Long> {

    Token findByLoginId(String loginId);
    Token findByLoginIdAndJwtToken(String loginId, String token);

}
