package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.OrderReport;
import ahc.dms.dao.dms.repositories.OrderReportRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class OrderReportService {

//	  *****************************JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************
	
    @Autowired
    private OrderReportRepository orderReportRepository;
//
//    public List<OrderReport> getOrderReports(Long fdId) {
//        return orderReportRepository.findActiveByFdId(fdId);
//    }
}
