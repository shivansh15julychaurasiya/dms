package ahc.dms.dao.dms.services;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import ahc.dms.dao.dms.entities.DownloadFile;
import ahc.dms.dao.dms.entities.DownloadReport;
import ahc.dms.dao.dms.repositories.DownloadFileRepository;
import ahc.dms.dao.dms.repositories.DownloadReportRepository;

@Service	
public class DownloadFileService {
	
//	  *****************************JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************
	
	@Autowired
	private DownloadReportRepository downloadReportRepository;
	
	@Autowired
	private DownloadFileRepository downloadFile;
	
	
	public DownloadReport getById(Long dr_id) {
		return downloadReportRepository.findById(dr_id).orElse(null);
	}
	public DownloadFile getFiles(Long dr_id) {
		
		return downloadFile.findById(dr_id).orElse(null);
		
	}

}
