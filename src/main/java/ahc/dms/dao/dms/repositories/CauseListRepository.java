package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CauseList;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;


@Repository
public interface CauseListRepository extends JpaRepository<CauseList, Long> {

    @Query("SELECT c FROM CauseList c " +
            "WHERE (c.cl_court_no = :courtNo) " +
            "AND ( c.cl_list_type_mid = :listTypeId) " +
            "AND ( c.cl_dol = :dol)")
    List<CauseList> searchCauseLists(
            @Param("courtNo") Integer courtNo,
            @Param("listTypeId") Long listTypeId,
            @Param("dol") LocalDate dol
    );
}



