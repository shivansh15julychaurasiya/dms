package ahc.dms.dao.respositories;

import ahc.dms.dao.entities.Token;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TokenRepository extends JpaRepository<Token, Long> {

    Token findByUsername(String username);
    Token findByUsernameAndJwtToken(String username, String token);

}
