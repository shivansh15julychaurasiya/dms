package ahc.dms.dao.dms.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import ahc.dms.dao.dms.entities.DownloadReport;

@Repository
public interface DownloadReportRepository  extends JpaRepository<DownloadReport,Long> {

}
