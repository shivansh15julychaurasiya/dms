package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.User;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {

    User findByName(String username);
    Optional<User> findByEmail(String email);
    Optional<User> findByLoginId(String username);

    boolean existsByEmail(String email);
    boolean existsByLoginId(String loginId);
    boolean existsByPhone(String phone);
}
