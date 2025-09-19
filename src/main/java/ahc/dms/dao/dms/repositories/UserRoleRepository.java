package ahc.dms.dao.dms.repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import ahc.dms.dao.dms.entities.Lookup;
import ahc.dms.dao.dms.entities.UserRole;

@Repository
public interface UserRoleRepository extends JpaRepository<UserRole, Long> {

    // Find by user and role
//    Optional<UserRole> findByUserAndRole(User user, Role role);
//    // Find all active roles for a user
//    List<UserRole> findByUserAndStatusTrue(User user);
//    // find active user-role mapping
//    boolean existsByUserAndRoleAndStatusTrue(User user, Role role);
//    // find by roles of a user
//    Set<UserRole> findByUser(User user);
//    // if user-role mapping exists
//    boolean existsByUserAndRole(User user, Role role);
    
    @Query("SELECT ur FROM UserRole ur WHERE ur.ur_um_mid = :userId AND ur.role = :role AND ur.ur_rec_status = 1")
    Optional<UserRole> findByUserIdAndRole(Long userId, Lookup role);

    @Query("SELECT ur FROM UserRole ur WHERE ur.ur_um_mid = :userId AND ur.ur_rec_status = 1")
    List<UserRole> findByUserIdAndStatusTrue(Long userId);

    @Query("SELECT CASE WHEN COUNT(ur) > 0 THEN true ELSE false END " +
           "FROM UserRole ur WHERE ur.ur_um_mid = :userId AND ur.role = :role AND ur.ur_rec_status = 1")
    boolean existsByUserIdAndRoleAndStatusTrue(Long userId, Lookup role);

    @Query("SELECT ur FROM UserRole ur WHERE ur.ur_um_mid = :userId")
    List<UserRole> findByUserId(Long userId);
    
}