package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.SubDocument;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface SubDocumentRepository extends JpaRepository<SubDocument, Long> {
	
	
	@Query(value = "SELECT * FROM sub_documents WHERE sd_fd_mid = :sdFdMid AND sd_if_mid = :sdIfMid AND sd_rec_status = :sdRecStatus ORDER BY sd_id ASC LIMIT 1", nativeQuery = true)
	Optional<SubDocument> findFirst(@Param("sdFdMid") Long sdFdMid, @Param("sdIfMid") int sdIfMid, @Param("sdRecStatus") int sdRecStatus);



}

