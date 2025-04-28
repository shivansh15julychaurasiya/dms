package ahc.dms.dao.pgdms.services;

import ahc.dms.dao.pgdms.entities.RequestLog;
import ahc.dms.dao.pgdms.repositories.RequestLogRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class RequestLogService {

    @Autowired
    private RequestLogRepository requestLogRepository;

    public void logRequest(String ipAddress, String requestURI, String method) {
        RequestLog requestLog = new RequestLog();
        requestLog.setIpAddress(ipAddress);
        requestLog.setEndpoint(requestURI);
        requestLog.setMethod(method);
        requestLogRepository.save(requestLog);
    }
}
