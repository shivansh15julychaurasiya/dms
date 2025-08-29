package ahc.dms.dao.dms.repositories;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import ahc.dms.dao.dms.entities.SubDocument;

public interface SubDocumentRepository extends JpaRepository<SubDocument, Long> {
	
	
	@Query(value = "SELECT * FROM sub_documents WHERE sd_fd_mid = :sdFdMid AND sd_if_mid = :sdIfMid AND sd_rec_status = :sdRecStatus ORDER BY sd_id ASC LIMIT 1", nativeQuery = true)
	Optional<SubDocument> findFirst(@Param("sdFdMid") Long sdFdMid, @Param("sdIfMid") int sdIfMid, @Param("sdRecStatus") int sdRecStatus);

	
	 @Query("SELECT d FROM SubDocument d " +
	           "WHERE d.sd_fd_mid = :fdId AND d.sd_submitted_date < :date " +
	           "AND d.sd_document_id = 100002 AND d.sd_rec_status = 1")
	    List<SubDocument> findOrdersBeforeDate(Long fdId, Date date);

	    @Query("SELECT d FROM SubDocument d " +
	           "WHERE d.sd_fd_mid = :fdId AND d.sd_submitted_date > :date " +
	           "AND d.sd_document_id = 100002 AND d.sd_rec_status = 4")
	    List<SubDocument> findOrdersForTransfer(Long fdId, Date date);
	    
	    
	    @Query("SELECT d FROM SubDocument d " +
	            "WHERE d.sd_fd_mid = :fd_id " +
	            "AND d.sd_submitted_date < :date " +
	            "AND d.sd_document_id = 100002 " +
	            "AND d.sd_rec_status = 1")
	     List<SubDocument> findSubDocumentOrders(@Param("fd_id") Long fdId, 
	                                             @Param("date") Date date);
	    
	    
	    // For getSubDocumentOrders
	    @Query("SELECT d FROM SubDocument d " +
	           "WHERE d.sd_fd_mid = :fdId " +
	           "AND d.sd_submitted_date < :date " +
	           "AND d.sd_document_id = 100002 " +
	           "AND d.sd_rec_status = 1")
	    List<SubDocument> getSubDocumentOrders(
	            @Param("fdId") Long fdId,
	            @Param("date") Date date
	    );

	    // For getSubDocumentOrdersForTransfer
	    @Query("SELECT d FROM SubDocument d " +
	           "WHERE d.sd_fd_mid = :fdId " +
	           "AND d.sd_submitted_date > :date " +
	           "AND d.sd_document_id = 100002 " +
	           "AND d.sd_rec_status = 4")
	    List<SubDocument> getSubDocumentOrdersForTransfer(
	            @Param("fdId") Long fdId,
	            @Param("date") Date date
	    );
	
	   

}

