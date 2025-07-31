package ahc.dms.dao.dms.services;

import ahc.dms.dao.dms.entities.CaseType;
import ahc.dms.dao.dms.repositories.CaseTypeRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class CaseTypeService {

    @Autowired
    private CaseTypeRepository ctr;


    public List<CaseType> getCaseTypes(){
        return ctr.findAll();
    }
}
