package ahc.dms.dao.pgdms.repositories;

import ahc.dms.dao.pgdms.entities.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {

    User findByName(String username);
    Optional<User> findByEmail(String email);
    Optional<User> findByLoginId(String username);

}
