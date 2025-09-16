package ahc.dms.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import ahc.dms.dao.dms.entities.OrderReport;
import ahc.dms.dao.dms.repositories.OrderReportRepository;

@RestController
@RequestMapping("/dms/orderreport")
public class OrderReportController {

    @Autowired
    private OrderReportRepository orderReportRepository;

    @GetMapping("/view/{fdId}")
    public ResponseEntity<List<OrderReport>> getOrderReports(@PathVariable Long fdId) {
        List<OrderReport> reports = orderReportRepository.findActiveOrderReportsByFdId(fdId);
        return ResponseEntity.ok(reports);
    }
}

