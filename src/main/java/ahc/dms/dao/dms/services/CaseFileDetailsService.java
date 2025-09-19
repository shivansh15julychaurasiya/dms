package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import ahc.dms.dao.dms.repositories.CaseFileDetailsRepository;
import ahc.dms.exceptions.ResourceNotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class CaseFileDetailsService {
	
//	  *****************************JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************
	
    private final CaseFileDetailsRepository repository;

    public List<CaseFileDetails> searchCases(Integer caseTypeId, String caseNo, Integer caseYear) {
        System.out.println("From Service-> Casetype="+caseTypeId+" "+"CaseNO="+caseNo+" "+"CaseYear="+caseYear);
        return repository.searchCases(caseTypeId, caseNo, caseYear);
    }

  

    public CaseFileDetails getCaseFileDetail(Long docId) {
        return repository.findById(docId).orElse(null);
    }
    
    public CaseFileDetails getCaseFileDetails(Long fdId) {
    	return repository.findById(fdId).orElse(null);
    }


}
