package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import ahc.dms.dao.dms.repositories.CaseFileDetailsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class CaseFileDetailServiceWrongFile {
    @Autowired
    private CaseFileDetailsRepository repository;

//    public List<CaseFileDetails> searchCases(String caseType, Long caseNo, Integer caseYear) {
//        return repository.findByCaseTypeAndCaseNoAndCaseYear(caseType, caseNo, caseYear);
//    }
}
