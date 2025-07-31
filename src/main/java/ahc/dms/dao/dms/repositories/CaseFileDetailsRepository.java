package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
@Repository
public interface CaseFileDetailsRepository extends JpaRepository<CaseFileDetails,Long> {

    @Query("SELECT c FROM CaseFileDetails c WHERE"
    +"(:caseType IS NULL OR c.caseType = :caseType) AND"
            + "(:caseNo IS NULL OR c.caseNo = :caseNo) AND"
    + "(:caseYear IS NULL OR c.caseYear = :caseYear)")

    List<CaseFileDetails> findByCaseTypeAndCaseNoAndCaseYear
            (@Param("caseType") String caseType,
             @Param("caseNo") Long caseNo,
             @Param("caseYear") Integer caseYear);

}
