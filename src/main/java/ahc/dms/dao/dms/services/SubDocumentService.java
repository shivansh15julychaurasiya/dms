package ahc.dms.dao.dms.services;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.expression.ParseException;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import ahc.dms.dao.dms.entities.ApplicationType;
import ahc.dms.dao.dms.entities.OrderFromElegalix;
import ahc.dms.dao.dms.entities.SubDocument;
import ahc.dms.dao.dms.repositories.ApplicationTypeRepository;
import ahc.dms.dao.dms.repositories.IndexFieldRepository;
import ahc.dms.dao.dms.repositories.SubDocumentRepository;
import io.jsonwebtoken.lang.Arrays;
import jakarta.transaction.Transactional;

@Service
public class SubDocumentService {

    private final SubDocumentRepository subDocumentRepository;
    private final CaseFileDetailsService caseFileDetailsService;
    private final RestTemplate restTemplate = new RestTemplate();
    private final IndexFieldRepository indexFieldRepository;
    private final ApplicationTypeRepository applicationTypeRepository;

    @Autowired
    public SubDocumentService(SubDocumentRepository subDocumentRepository,CaseFileDetailsService caseFileDetailsService, IndexFieldRepository indexFieldRepository,
    		ApplicationTypeRepository applicationTypeRepository) {
        this.subDocumentRepository = subDocumentRepository;
        this.caseFileDetailsService=caseFileDetailsService;
        this.indexFieldRepository=indexFieldRepository;
        this.applicationTypeRepository=applicationTypeRepository;
    }

    
    public SubDocument getSubDocumentById(Long sd_Id) {
    	return subDocumentRepository.findById(sd_Id).orElse(null);
    }
    
    @Transactional
    public Optional<SubDocument> getPetitionSubDocument(Long fdId, Integer recStatus) {
    	
        return subDocumentRepository
                .findFirst(fdId,1,recStatus);
                
    }
    
    
    @Transactional
    public List<SubDocument> getOrdersFromElegalix(Long fdId) throws java.text.ParseException {
        List<SubDocument> subDocuments = new ArrayList<>();

        // Step 1: fetch case details
        var cfd = caseFileDetailsService.getCaseFileDetail(fdId).orElse(null);
        if (cfd == null) return subDocuments;

        // Step 2: Example date cutoff
        Date cutoffDate = null;
        try {
            cutoffDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
                    .parse("2017-08-17 00:00:00");
        } catch (ParseException e) {
            throw new RuntimeException("Error parsing date", e);
        }

        // Step 3: Fetch documents from DMS
        subDocuments.addAll(Optional.ofNullable(subDocumentRepository.getSubDocumentOrders(fdId, cutoffDate))
                .orElse(Collections.emptyList()));
        subDocuments.addAll(Optional.ofNullable(subDocumentRepository.getSubDocumentOrdersForTransfer(fdId, cutoffDate))
                .orElse(Collections.emptyList()));

        // Step 4: Call Elegalix API (simplified)
        String uri = "http://192.168.0.162:8080/elegalix_restapi2/api/judgments/A/"
                + cfd.getCaseType().getLabel() + "/"
                + cfd.getFdCaseNo() + "/"
                + cfd.getFdCaseYear();

        System.out.println("CaseNo"+cfd.getFdCaseNo()+"- caseyear"+cfd.getFdCaseYear());
        
        try {
            ResponseEntity<OrderFromElegalix[]> response =
                    restTemplate.getForEntity(uri, OrderFromElegalix[].class);

            if (response.getBody() != null) {
                Arrays.asList(response.getBody()).forEach(order -> {
                    SubDocument sb = new SubDocument();
                    sb.setJudgmentID(order.getJudgmentID());
                    sb.setSd_document_id(100002);
                    sb.setSd_if_mid(39L);
                    sb.setSd_submitted_date(order.getJudgmentDate());
                    sb.setSd_rec_status(1);
                    sb.setSd_fd_mid(fdId);
                    sb.setIndexField(indexFieldRepository.findById(34L).orElse(null));
                    sb.setChecked(false);
                    sb.setSd_nonmaintainable(false);
                    sb.setDocumentType(applicationTypeRepository.findById((long) 100002).orElse(null));
                    sb.setSd_document_name(order.getJudgmentID().toString());
                    sb.setSubApplications(new ArrayList<>());
                    subDocuments.add(sb);
                });
            }

        } catch (Exception e) {
            e.printStackTrace(); // Better: log.error("Elegalix API failed", e);
        }

        return subDocuments;
    }
    
    
   

	
    
    
}
