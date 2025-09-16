package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface CaseFileDetailsRepository extends JpaRepository<CaseFileDetails, Long> {

    @Query("SELECT c FROM CaseFileDetails c WHERE "
            + "(c.caseType.id = :caseTypeId) AND "
            + "(c.fdCaseNo = :caseNo) AND "
            + "(c.fdCaseYear = :caseYear)")
    List<CaseFileDetails> searchCases(
            @Param("caseTypeId") Integer caseTypeId,
            @Param("caseNo") String caseNo,
            @Param("caseYear") Integer caseYear
    );


    @Query("SELECT c FROM CaseFileDetails c WHERE c.caseType.id = :caseTypeId AND c.caseNo = :caseNo AND c.caseYear = :caseYear")
    Optional<CaseFileDetails> findByCaseTypeAndCaseNoAndCaseYear(String caseType , String caseNo, Integer caseYear);
    
}
