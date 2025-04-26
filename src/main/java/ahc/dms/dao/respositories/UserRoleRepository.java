package ahc.dms.dao.respositories;

import ahc.dms.dao.entities.Role;
import ahc.dms.dao.entities.User;
import ahc.dms.dao.entities.UserRole;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface UserRoleRepository extends JpaRepository<UserRole, Long> {

    // Find by user and role
    Optional<UserRole> findByUserAndRole(User user, Role role);
    // Find all active roles for a user
    List<UserRole> findByUserAndStatusTrue(User user);
    // find active user-role mapping
    boolean existsByUserAndRoleAndStatusTrue(User user, Role role);
}