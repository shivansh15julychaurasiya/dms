package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CourtMaster;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CourtMasterRepository extends JpaRepository<CourtMaster,Long> {
}
