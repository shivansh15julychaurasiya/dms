package ahc.dms.dao.dms.repositories;



import ahc.dms.dao.dms.entities.CaseType;
import ahc.dms.dao.dms.entities.Documents;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface CaseTypeRepository extends JpaRepository<CaseType, Integer> {

    Optional<CaseType> findByLabel(String label);

    // You can add more query methods if needed




}
