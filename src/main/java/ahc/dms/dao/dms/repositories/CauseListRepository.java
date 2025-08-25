package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CauseList;
import ahc.dms.dao.dms.entities.CauseListType;
import ahc.dms.dao.dms.entities.CourtUserMapping;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

import java.text.SimpleDateFormat;
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

    @Query("SELECT c FROM CauseListType c " +
            "WHERE ( c.clt_id = :listTypeId) ")
      //      +"AND ( c.cl_dol = :dol)")
    CauseListType searchListType(
            @PathVariable Long listTypeId
//            @Param("listTypeId") Long listTypeId,
//            @Param("dol") LocalDate dol
    );
// this running query
//@Query(value = """
//    SELECT * FROM cause_list c
//    WHERE c.cl_rec_status = 1
//      AND c.cl_dol = :clDol
//      AND c.cl_court_no = :clCourtNo
//      OR (
//           c.cl_list_type_mid = :clListTypeMid
//           OR c.cl_list_type_mid IN (5, 6, 7)
//      )
//    ORDER BY c.cl_court_no, c.cl_serial_no, c.cl_id
//    """, nativeQuery = true)
//List<CauseList> getCauseLists(
//        @Param("clDol") LocalDate clDol,
//        @Param("clCourtNo") Integer clCourtNo,
//        @Param("clListTypeMid") Long clListTypeMid
//);

// this is new query for list type mid
@Query(value = """
  (
    SELECT * FROM cause_list c
    WHERE c.cl_rec_status = 1
      AND c.cl_dol = :clDol
      AND c.cl_court_no = :clCourtNo
      AND (c.cl_list_type_mid IS NULL OR c.cl_list_type_mid = :clListTypeMid)
  )
  UNION ALL
  (
    SELECT * FROM cause_list c
    WHERE c.cl_rec_status = 1
      AND c.cl_dol = :clDol
      AND c.cl_court_no = :clCourtNo
      AND c.cl_list_type_mid IN (5, 6, 7)
      AND NOT EXISTS (
        SELECT 1 FROM cause_list c2
        WHERE c2.cl_rec_status = 1
          AND c2.cl_dol = :clDol
          AND c2.cl_court_no = :clCourtNo
          AND (c2.cl_list_type_mid IS NULL OR c2.cl_list_type_mid = :clListTypeMid)
      )
  )
  ORDER BY cl_court_no, cl_serial_no, cl_id
""", nativeQuery = true)
List<CauseList> getCauseLists(
        @Param("clDol") LocalDate clDol,
        @Param("clCourtNo") Integer clCourtNo,
        @Param("clListTypeMid") Long clListTypeMid
);



//    @Query(value = """
//        SELECT COUNT(cl_serial_no) AS count, cl_list_type_mid AS clListTypeMid
//        FROM cause_list
//        WHERE cl_dol = :dol
//          AND (cl_court_no = :courtNo)
//          AND cl_rec_status = 1
//        GROUP BY cl_list_type_mid
//        """, nativeQuery = true)
//    List<Object[]> getByListType(
//            @Param("dol") LocalDate dol,
//            @Param("courtNo") Integer courtNo
//    );

      @Query(value = """
    SELECT COUNT(c.cl_serial_no) AS count,
    c.cl_list_type_mid AS clListTypeMid,
    t.clt_description AS listTypeName
    FROM cause_list c
    JOIN cause_list_type t ON t.clt_id = c.cl_list_type_mid
    WHERE DATE(c.cl_dol) = :dol
    AND c.cl_court_no = :courtNo
    AND c.cl_rec_status = 1
    GROUP BY c.cl_list_type_mid, t.clt_description
    """, nativeQuery = true)
    List<Object[]> getByListType(
            @Param("dol") LocalDate dol,
            @Param("courtNo") Integer courtNo
    );











}



