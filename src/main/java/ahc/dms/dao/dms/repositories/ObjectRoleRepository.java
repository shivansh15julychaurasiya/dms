package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.ObjectMaster;
import ahc.dms.dao.dms.entities.ObjectRole;
import ahc.dms.dao.dms.entities.Lookup;   //  replace Role with Lookup
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ObjectRoleRepository extends JpaRepository<ObjectRole, Long> {

    boolean existsByObjectMasterAndRole(ObjectMaster objectMaster, Lookup role);

    Optional<ObjectRole> findByObjectMasterAndRole(ObjectMaster om, Lookup role);

    Optional<ObjectRole> findByObjectMasterAndRoleAndStatusTrue(ObjectMaster existingOm, Lookup role);
}
