package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.Token;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TokenRepository extends JpaRepository<Token, Long> {

    Token findByUsername(String username);
    Token findByUsernameAndJwToken(String username, String token);

}
