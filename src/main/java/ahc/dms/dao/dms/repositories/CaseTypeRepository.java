package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CaseType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CaseTypeRepository extends JpaRepository<CaseType,Long> {
}
