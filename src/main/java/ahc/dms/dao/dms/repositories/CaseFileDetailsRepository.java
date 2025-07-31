package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CaseFileDetailsRepository extends JpaRepository<CaseFileDetails, Long> {

    @Query("SELECT c FROM CaseFileDetails c WHERE "
            + "(:caseTypeId IS NULL OR c.caseType.id = :caseTypeId) AND "
            + "(:caseNo IS NULL OR c.fdCaseNo = :caseNo) AND "
            + "(:caseYear IS NULL OR c.fdCaseYear = :caseYear)")
    List<CaseFileDetails> searchCases(
            @Param("caseTypeId") Integer caseTypeId,
            @Param("caseNo") String caseNo,
            @Param("caseYear") Integer caseYear
    );
}
