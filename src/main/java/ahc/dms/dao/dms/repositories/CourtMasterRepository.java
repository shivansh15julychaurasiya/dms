package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CourtMaster;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface CourtMasterRepository extends JpaRepository<CourtMaster, Integer> {

//    @Modifying
//    @Query("UPDATE CourtMaster c SET c.cm_bench_id = :benchId, c.cm_mod_date = CURRENT_TIMESTAMP WHERE c.cm_id = :courtId")
//    CourtMaster updateBenchId(@Param("courtId") Integer courtId, @Param("benchId") Integer benchId);

    // No @Query or @Modifying
    Optional<CourtMaster> findById(Integer id);

    Optional<CourtMaster> findByCmBenchId(Integer benchId); // Find by benchId



}

