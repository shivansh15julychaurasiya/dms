package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CauseList;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.Date;
import java.util.List;

@Repository
public interface CauseListRepository extends JpaRepository<CauseList, Long> {

    @Query("SELECT c FROM CauseList c " +
            "JOIN c.courtMaster cm " +
            "JOIN c.clType clt " +
            "WHERE (:courtName IS NULL OR LOWER(cm.cm_name) = LOWER(:courtName)) " +
            "AND (:desc IS NULL OR LOWER(clt.clt_description) LIKE LOWER(CONCAT('%', :desc, '%'))) " +
            "AND (:date IS NULL OR DATE(c.cl_date) = DATE(:date))")
    List<CauseList> searchByCourtNameAndListTypeDescAndDate(
            @Param("courtName") String courtName,
            @Param("desc") String desc,
            @Param("date") Date date);


}

