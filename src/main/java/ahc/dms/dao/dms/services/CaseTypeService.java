package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CaseType;
import ahc.dms.dao.dms.repositories.CaseTypeRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class CaseTypeService {
	
	
//	  *****************************JAVA FULLSTACK DEVELOPER VIJAY DEVELOPER *******************************

    @Autowired
    private CaseTypeRepository caseTypeRepository;


    public List<CaseType> getAllCaseTypes() {
        return caseTypeRepository.findAll();
    }


    public Optional<CaseType> getById(Integer id) {
        return caseTypeRepository.findById(id);
    }


    public Optional<CaseType> getByLabel(String label) {
        return caseTypeRepository.findByLabel(label);
    }
}
