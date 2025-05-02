package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.ObjectMaster;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ObjectMasterRepository extends JpaRepository<ObjectMaster, Long> {
    Optional<ObjectMaster> findByRequestUriAndRequestMethodAndStatusTrue(String uri, String method);
    Optional<ObjectMaster> findByRequestUriStartingWithAndRequestMethodAndStatusTrue(String uri, String method);
}