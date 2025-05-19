package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.ObjectMaster;
import ahc.dms.dao.dms.entities.ObjectRole;
import ahc.dms.dao.dms.entities.Role;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ObjectRoleRepository extends JpaRepository<ObjectRole, Long> {

    boolean existsByObjectMasterAndRole(ObjectMaster objectMaster, Role adminRole);
}
