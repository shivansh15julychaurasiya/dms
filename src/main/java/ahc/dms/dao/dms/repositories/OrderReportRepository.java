package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.OrderReport;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.List;

public interface OrderReportRepository extends JpaRepository<OrderReport, Long> {

    @Query("SELECT o FROM OrderReport o JOIN FETCH User u ON u.userId = o.ord_created_by WHERE o.ordFdMid = :fdId AND o.ord_rec_status = 1")
    List<OrderReport> findActiveOrderReportsByFdId(@Param("fdId") Long fdId);
    
//    @Query("SELECT o FROM OrderReport o  WHERE o.ordFdMid = :fdId AND o.ord_rec_status = 1")
//    List<OrderReport> findActiveOrderReportsByFdId(@Param("fdId") Long fdId);
    
//    @Query("SELECT o FROM OrderReport o WHERE o.ordFdMid = :fdId AND o.ord_rec_status = 1")
//    List<OrderReport> findActiveOrderReportsByFdId(@Param("fdId") Long fdId);

    
}
