package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.Documents;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface DocumentRepository extends JpaRepository<Documents, Long> {



    @Query("SELECT d FROM Documents d WHERE " +
            "(:caseTypeLabel IS NULL OR LOWER(TRIM(d.caseType)) = LOWER(TRIM(:caseTypeLabel))) AND " +
            "(:caseNo IS NULL OR TRIM(d.caseNumber) = TRIM(:caseNo)) AND " +
            "(:caseYear IS NULL OR TRIM(d.caseYear) = TRIM(:caseYear))")
    List<Documents> searchDocuments(
            @Param("caseTypeLabel") String caseTypeLabel,
            @Param("caseNo") String caseNo,
            @Param("caseYear") String caseYear
    );



}
