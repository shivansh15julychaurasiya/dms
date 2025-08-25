package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CourtUserMapping;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface CourtUserMappingRepository extends JpaRepository<CourtUserMapping, Long> {

    @Query("SELECT c FROM CourtUserMapping c " +
            "WHERE(c.CumUserMid = :userId )")
    CourtUserMapping getCourtMapping(
            @Param("userId") Long userId
    );
}
