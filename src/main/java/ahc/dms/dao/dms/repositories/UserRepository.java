package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.User;
import ahc.dms.payload.dto.LookupDto;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.springframework.data.domain.Page;       //  Correct
import org.springframework.data.domain.Pageable;

import java.util.Optional;
import java.util.Set;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {

    User findByName(String username);
    Optional<User> findByEmail(String email);
    Optional<User> findByUsername(String username);


    boolean existsByEmail(String email);
    boolean existsByUsername(String username);
    boolean existsByPhone(String phone);
    
    



    Page<User> findAll(Pageable pageable);
}
