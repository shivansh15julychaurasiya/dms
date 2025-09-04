package ahc.dms.dao.dms.repositories;

import org.springframework.data.jpa.repository.JpaRepository;

import ahc.dms.dao.dms.entities.DownloadFile;

public interface DownloadFileRepository extends JpaRepository<DownloadFile,Long> {

}
